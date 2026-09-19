# 04_audit_exact_matches.R
# Audit of the "exact" matches: urban stations geocoded far from the center of
# their own town are geocoded again, and then corrected or downgraded.
#
# Input:  geocodificacion_final.csv, centroides_localidad.csv
# Output: geocodificacion_final.csv (overwritten in place)
#         fix_cola_cache.csv        (query results; lets the run be resumed)
#
# The distance from each exact match to the centroid of its own locality shows a
# tail of urban stations more than 10-20 km from the center of their town: a common
# street name ("Roca", "San Martín", "25 de Mayo") was matched in the wrong town of
# the same (large) department, which the department check of the first pass cannot
# catch.
#
# The tail is geocoded again with the structured search of Nominatim, with the city
# set to the locality and a tight box around the locality centroid, which forces
# the right town. For each station:
#   - new point within 15 km of the locality centroid: coordinates corrected, the
#     station stays exact;
#   - no such point, and the original was more than 20 km away (clearly wrong):
#     downgraded to the locality centroid;
#   - no such point, and the original was 10-20 km away (may be a large city): left
#     unchanged.
# Urban addresses only; highway addresses are handled in 05_geocode_route_km.R.
#
# Test run on 8 stations, which leaves the final file untouched:
#   GEO_N_TEST=8 Rscript code/02_geocoding/04_audit_exact_matches.R

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(fs)
})
source("code/00_config.R")
options(timeout = 60)

DIR_INT <- DIR_INTERIM
FIN   <- fs::path(DIR_INT, "geocodificacion_final.csv")
CENTL <- fs::path(DIR_INT, "centroides_localidad.csv")
CACHE <- fs::path(DIR_INT, "fix_cola_cache.csv")
UA <- sprintf("fuel-retail-geocoder/1.0 (academic research; %s)", CONTACT_EMAIL)
# Thresholds, in km from the locality centroid
UMBRAL_COLA <- 10   # Urban exact matches farther than this are geocoded again
GATE_OK     <- 15   # The new point is accepted if it falls within this distance
UMBRAL_MAL  <- 20   # Downgrade to locality only if the original was farther than this (clearly wrong)

norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))
haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371.0; p <- pi/180
  dlat <- (lat2-lat1)*p; dlon <- (lon2-lon1)*p
  a <- sin(dlat/2)^2 + cos(lat1*p)*cos(lat2*p)*sin(dlon/2)^2
  2*R*asin(pmin(1, sqrt(a)))
}

# Address parser (same as in 02_geocode_structured_search.R) ----
limpia_dir <- function(x){
  s0 <- as.character(x)
  s0 <- gsub("([a-z])([A-Z])", "\\1 \\2", s0)
  if (!grepl(" ", trimws(s0))) {
    s0 <- gsub("([A-Za-z])([0-9])", "\\1 \\2", s0); s0 <- gsub("([0-9])([A-Za-z])", "\\1 \\2", s0)
  }
  s0 <- gsub("Â", "", s0); s0 <- gsub("N[º°]", " ", s0); s0 <- gsub("\\bNro\\.?\\b", " ", s0, ignore.case = TRUE)
  s <- norm(s0)
  s <- gsub("\\(.*?\\)", " ", s); s <- gsub("\\s+-\\s+[A-Z ]+$", " ", s)
  s <- gsub("\\bAVDA\\.?\\b|\\bAV\\.", "AV ", s); s <- gsub("\\bGRAL\\.?\\b", "GENERAL ", s)
  s <- gsub("\\bBV\\.?\\b|\\bBVAR\\.?\\b", "BOULEVARD ", s)
  s <- gsub("\\bESQ\\.?\\b|\\bESQUINA\\b", " Y ", s); s <- gsub("S/N[ºO]?|SIN NUMERO", " ", s)
  s <- gsub("[^A-Z0-9 ]", " ", s)
  gsub("\\s+", " ", trimws(s))
}
es_interseccion <- function(s) grepl(" Y | E ", s)
GENERICOS <- c("", "AV", "AVENIDA", "CALLE", "CALLES", "BOULEVARD", "DIAGONAL", "RUTA")
parse_calle_altura <- function(d){
  s <- limpia_dir(d)
  if (es_interseccion(s)) s <- trimws(strsplit(s, " Y | E ")[[1]][1])
  s <- trimws(gsub("\\s+(OESTE|ESTE|NORTE|SUR)\\s*$", "", s))
  m <- regmatches(s, regexpr("[0-9]{1,6}\\s*$", s))
  if (length(m) == 1L && nzchar(m)) {
    altura <- trimws(m); resto <- trimws(sub("[0-9]{1,6}\\s*$", "", s))
    if (norm(resto) %in% GENERICOS) return(list(calle = s, altura = ""))
    return(list(calle = resto, altura = altura))
  }
  list(calle = s, altura = "")
}

# Structured query anchored to the city ----
# Tight box around the locality centroid: +/- 0.22 degrees (0.45 in the earlier passes)
nomi_ciudad <- function(calle, altura, ciudad, depto, prov, llat, llon, d = 0.22){
  street <- trimws(paste(altura, calle))
  p <- c(sprintf("street=%s", utils::URLencode(street, reserved = TRUE)),
         sprintf("city=%s",   utils::URLencode(as.character(ciudad), reserved = TRUE)),
         sprintf("state=%s",  utils::URLencode(as.character(prov), reserved = TRUE)),
         "country=Argentina", "format=json", "limit=1")
  if (!is.na(depto) && nzchar(as.character(depto)))
    p <- c(p, sprintf("county=%s", utils::URLencode(as.character(depto), reserved = TRUE)))
  p <- c(p, sprintf("viewbox=%f,%f,%f,%f", llon-d, llat+d, llon+d, llat-d), "bounded=1")
  u <- paste0("https://nominatim.openstreetmap.org/search?", paste(p, collapse = "&"))
  r <- tryCatch({ con <- url(u, headers = c("User-Agent" = UA)); on.exit(close(con), add = TRUE)
                  jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "")) },
                error = function(e) NULL)
  Sys.sleep(1.1)                                  # Usage policy: 1 request per second
  if (is.null(r) || length(r) == 0 || (is.data.frame(r) && nrow(r) == 0)) return(NULL)
  list(lat = as.numeric(r$lat[1]), lon = as.numeric(r$lon[1]), nom = as.character(r$display_name[1]))
}

# Tail of suspicious exact matches ----
# Tail (cola): urban exact matches farther than UMBRAL_COLA from their locality centroid
g  <- fread(FIN,   encoding = "UTF-8")
cl <- fread(CENTL, encoding = "UTF-8")[!is.na(llat), .(provincia, localidad, llat, llon)]
g <- merge(g, cl, by = c("provincia","localidad"), all.x = TRUE, sort = FALSE)
g[!is.na(llat), d_loc := haversine(lat, lon, llat, llon)]

cola <- g[precision == "exacta" & tipo == "urbana" & !is.na(d_loc) & d_loc > UMBRAL_COLA]
cat("Tail to check (urban exact matches more than", UMBRAL_COLA, "km from the town center):", nrow(cola), "\n")

# Resume from the cache if there is one
if (fs::file_exists(CACHE)) {
  ya <- fread(CACHE, encoding = "UTF-8")
  cola <- cola[!nro_inscripcion %in% ya$nro_inscripcion]
  cat("Resuming, pending:", nrow(cola), "\n")
} else ya <- NULL
.n_test <- suppressWarnings(as.integer(Sys.getenv("GEO_N_TEST", "0")))
if (!is.na(.n_test) && .n_test > 0) {
  set.seed(3); cola <- cola[sample(.N, min(.n_test, .N))]
  cat("Test mode:", nrow(cola), "stations\n")
}

acum <- vector("list", nrow(cola))
for (i in seq_len(nrow(cola))) {
  r <- cola[i]; z <- parse_calle_altura(r$direccion)
  h <- nomi_ciudad(z$calle, z$altura, r$localidad, r$departamento, r$provincia, r$llat, r$llon)
  if (!is.null(h)) {
    d1 <- haversine(h$lat, h$lon, r$llat, r$llon)
    acum[[i]] <- data.table(nro_inscripcion = r$nro_inscripcion, newlat = h$lat, newlon = h$lon,
                            d1 = round(d1,2), d0 = round(r$d_loc,2), found = TRUE,
                            detalle = substr(h$nom, 1, 80))
  } else {
    acum[[i]] <- data.table(nro_inscripcion = r$nro_inscripcion, newlat = NA_real_, newlon = NA_real_,
                            d1 = NA_real_, d0 = round(r$d_loc,2), found = FALSE, detalle = "")
  }
  if (i %% 25 == 0 || i == nrow(cola)) {
    out <- rbindlist(c(list(ya), acum[seq_len(i)]), use.names = TRUE, fill = TRUE)
    fwrite(out, CACHE, na = "NA", bom = TRUE)
    cat(sprintf("  %4d/%d\n", i, nrow(cola)))
  }
}
res <- fread(CACHE, encoding = "UTF-8")

# In test mode print what the API returned and stop before touching the final file
if (!is.na(.n_test) && .n_test > 0) {
  cat("\nTest mode results (nothing is overwritten)\n")
  print(res[, .(nro_inscripcion, d0, d1, found, detalle = substr(detalle,1,50))])
  quit(status = 0)
}

# Apply the decisions to geocodificacion_final.csv ----
g[, orig_prec := precision]
corr <- res[found == TRUE & d1 <= GATE_OK]                       # Corrected: new point in the right town
setkey(g, nro_inscripcion)
for (i in seq_len(nrow(corr))) {
  ni <- corr$nro_inscripcion[i]
  g[.(ni), `:=`(lat = corr$newlat[i], lon = corr$newlon[i],
                fuente = "nominatim_ciudad_fix", precision = "exacta",
                detalle = corr$detalle[i])]
}
# Downgrade to locality: no valid new point and the original was clearly wrong (>20 km)
baja_ni <- res[(found == FALSE | d1 > GATE_OK)]$nro_inscripcion
baja_ni <- intersect(baja_ni, g[d_loc > UMBRAL_MAL]$nro_inscripcion)
baja_ni <- setdiff(baja_ni, corr$nro_inscripcion)
for (ni in baja_ni) {
  g[.(ni), `:=`(lat = llat, lon = llon, fuente = "centroide_localidad",
                precision = "localidad", km_al_centroide = 0, detalle = "bajada de exacta erronea")]
}
n_corr <- nrow(corr); n_baja <- length(baja_ni)
n_deja <- nrow(res) - n_corr - n_baja
cat(sprintf("\nDecisions\n  Corrected (new point inside the town): %d\n  Downgraded to locality (wrong and not recoverable): %d\n  Left unchanged (10-20 km, possibly a large city): %d\n",
    n_corr, n_baja, n_deja))

# Distance to the town center after the corrections, for the summary below
g[!is.na(llat), d_loc2 := haversine(lat, lon, llat, llon)]
cat("\nDistance to the town center, urban exact matches: before vs after\n")
au <- g[orig_prec == "exacta" & tipo == "urbana" & !is.na(d_loc)]
cat(sprintf("  >20 km:  before %d, after %d\n", au[d_loc>20,.N], au[precision=="exacta" & d_loc2>20,.N]))
cat(sprintf("  >10 km:  before %d, after %d\n", au[d_loc>10,.N], au[precision=="exacta" & d_loc2>10,.N]))
cat(sprintf("  p99 (km): before %.1f, after %.1f\n",
    quantile(au$d_loc,.99,na.rm=TRUE), quantile(g[precision=="exacta"&tipo=="urbana"]$d_loc2,.99,na.rm=TRUE)))
cat("\nNew breakdown by precision\n"); print(g[, .N, by=precision][order(-N)])

# Save with the same columns as the input file
cols <- c("provincia","localidad","nro_inscripcion","departamento","direccion","tipo",
          "lat","lon","fuente","precision","km_al_centroide","detalle")
fwrite(g[, ..cols], FIN, na = "NA", bom = TRUE)
cat("\ngeocodificacion_final.csv overwritten\n")

# 01_geocode_addresses.R
# First geocoding pass: latitude and longitude of every retail station, kept in a
# table of its own. The analysis panel is only read, to list the unique addresses.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: geocodificacion_estaciones.csv (one row per boca (outlet))
#         centroides_departamento.csv    (department centroids, reused as a cache)
#
# Each station goes down a cascade:
#   1. georef /direcciones, the address API of the national government: exact
#      point, taken from the first candidate that lies in the station's department.
#   2. Nominatim (OpenStreetMap): exact point, searched inside a box around the
#      department centroid.
#   3. Department centroid: approximate, and flagged as such.
# Match rates on a test sample: georef about 40% of urban addresses and Nominatim
# about 70% (the two complement each other); about 25% of highway addresses,
# counting verified matches only. Whatever is left stays approximate.
#
# The run can be resumed: if the output file exists, the stations already in it
# are skipped. Nominatim allows 1 request per second, so a full run takes about
# 2.5 hours.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(fs)
})
source("code/00_config.R")
options(timeout = 60)

DIR_INT  <- DIR_INTERIM
FILE_IN  <- fs::path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
OUT_GEO  <- fs::path(DIR_INT, "geocodificacion_estaciones.csv")
OUT_CENT <- fs::path(DIR_INT, "centroides_departamento.csv")
UA <- sprintf("fuel-retail-geocoder/1.0 (academic research; %s)", CONTACT_EMAIL)

# Helpers ----

# Upper case without accents, to compare names across sources
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))
GJ   <- function(u) tryCatch(jsonlite::fromJSON(u), error = function(e) NULL)

# Clean an address: drop parentheses and neighborhood suffixes, expand abbreviations
limpia_dir <- function(x){
  s <- norm(x)
  s <- gsub("\\(.*?\\)", " ", s)                 # "(esq. R. Carrillo)"
  s <- gsub("\\s+-\\s+[A-Z ]+$", " ", s)         # " - BANFIELD"
  s <- gsub("N[º°]|NRO\\.?", " ", s)
  s <- gsub("\\bAVDA\\.?\\b|\\bAV\\.", "AV ", s)
  s <- gsub("\\bGRAL\\.?\\b", "GENERAL ", s)
  s <- gsub("\\bBV\\.?\\b|\\bBVAR\\.?\\b", "BOULEVARD ", s)
  s <- gsub("\\bESQ\\.?\\b|\\bESQUINA\\b", " Y ", s)
  s <- gsub("S/N[ºO]?|SIN NUMERO", " ", s)
  s <- gsub("[^A-Z0-9 ]", " ", s)
  gsub("\\s+", " ", trimws(s))
}

# Department check. For the city of Buenos Aires georef returns "Comuna N" while
# the crosswalk has "CABA".
dep_match <- function(dep_ours, dep_geo){
  a <- norm(dep_ours); b <- norm(dep_geo)
  # No match if either name is missing
  if (length(a) != 1L || length(b) != 1L || is.na(a) || is.na(b)) return(FALSE)
  if (a == "CABA" || grepl("CAPITAL FEDERAL", a))
    return(isTRUE(grepl("^COMUNA", b) || grepl("CAPITAL", b)))
  isTRUE(a == b)
}

# Distance in km on a flat approximation (111 km per degree)
km_ent <- function(lat1, lon1, lat2, lon2)
  111 * sqrt((lat1-lat2)^2 + ((lon1-lon2) * cos(lat1*pi/180))^2)

# 1. Unique stations ----
# One address per station: the first one that appears in the panel
b <- readRDS(FILE_IN); setDT(b)
est <- unique(b[canal_de_comercializacion == "Al público",
                .(nro_inscripcion, direccion, localidad, provincia, departamento)],
              by = "nro_inscripcion")
est <- est[!is.na(direccion) & trimws(direccion) != ""]
rm(b); invisible(gc())
est[, ruta := grepl("\\bRUTA\\b|\\bRN\\b|\\bRP\\b|\\bAUTOPISTA\\b|\\bAUTOVIA\\b|\\bKM\\b", norm(direccion))]
cat("Stations to geocode:", nrow(est), "| on highways:", sum(est$ruta), "\n")

# 2. Province ids ----
pv <- GJ("https://apis.datos.gob.ar/georef/api/provincias?max=30")$provincias
p_id <- as.character(pv$id); p_nom <- norm(pv$nombre)
mapid <- setNames(p_id, p_nom)
# The panel names these two provinces differently from georef
mapid[["CAPITAL FEDERAL"]]  <- p_id[grepl("AUTONOMA", p_nom)][1]
mapid[["TIERRA DEL FUEGO"]] <- p_id[grepl("TIERRA DEL FUEGO", p_nom)][1]
est[, prov_id := mapid[norm(provincia)]]

# 3. Department centroids (cached) ----
deps <- unique(est[, .(provincia, departamento, prov_id)])
if (fs::file_exists(OUT_CENT)) {
  cent <- fread(OUT_CENT, encoding = "UTF-8")
  cat("Cached centroids:", nrow(cent), "\n")
} else {
  cat("Downloading centroids of", nrow(deps), "departments...\n")
  cent <- rbindlist(lapply(seq_len(nrow(deps)), function(i){
    u <- sprintf("https://apis.datos.gob.ar/georef/api/departamentos?nombre=%s&provincia=%s&max=1&campos=centroide",
                 utils::URLencode(as.character(deps$departamento[i]), reserved = TRUE), deps$prov_id[i])
    r <- GJ(u); Sys.sleep(0.15)
    la <- lo <- NA_real_
    if (!is.null(r) && length(r$departamentos) > 0 && nrow(r$departamentos) > 0) {
      la <- tryCatch(as.numeric(r$departamentos$centroide$lat[1]), error = function(e) NA_real_)
      lo <- tryCatch(as.numeric(r$departamentos$centroide$lon[1]), error = function(e) NA_real_)
    }
    if (i %% 50 == 0) cat("  ", i, "/", nrow(deps), "\n")
    data.table(provincia = deps$provincia[i], departamento = deps$departamento[i], clat = la, clon = lo)
  }))
  fwrite(cent, OUT_CENT, na = "NA", bom = TRUE)
}
est <- merge(est, cent, by = c("provincia","departamento"), all.x = TRUE)

# 4. Geocoders ----

# Query georef: up to 5 candidates, keep the first that passes the department check
geo_georef <- function(dir, prov_id, dep_ours, loc){
  intento <- function(u){
    r <- GJ(u); Sys.sleep(0.2)
    if (is.null(r) || length(r$direcciones) == 0 || nrow(r$direcciones) == 0) return(NULL)
    dd  <- r$direcciones
    lat <- tryCatch(dd$ubicacion$lat, error = function(e) rep(NA, nrow(dd)))
    lon <- tryCatch(dd$ubicacion$lon, error = function(e) rep(NA, nrow(dd)))
    dep <- tryCatch(dd$departamento$nombre, error = function(e) rep(NA, nrow(dd)))
    ok  <- which(!is.na(lat) & vapply(seq_along(dep), function(k) dep_match(dep_ours, dep[k]), logical(1)))
    if (!length(ok)) return(NULL)
    list(lat = as.numeric(lat[ok[1]]), lon = as.numeric(lon[ok[1]]), dep = as.character(dep[ok[1]]))
  }
  base_u <- sprintf("https://apis.datos.gob.ar/georef/api/direcciones?direccion=%s&provincia=%s&max=5",
                    utils::URLencode(dir, reserved = TRUE), prov_id)
  z <- intento(paste0(base_u, "&localidad=", utils::URLencode(as.character(loc), reserved = TRUE)))
  if (is.null(z)) z <- intento(base_u)          # Retry without the locality; the department check still applies
  z
}

# Nominatim free-text search, within +/- d degrees of the department centroid
geo_nominatim <- function(q, clat, clon, d = 0.45){
  vb <- if (is.na(clat)) "" else sprintf("&viewbox=%f,%f,%f,%f&bounded=1", clon-d, clat+d, clon+d, clat-d)
  u  <- paste0("https://nominatim.openstreetmap.org/search?format=json&limit=1&countrycodes=ar&q=",
               utils::URLencode(q, reserved = TRUE), vb)
  r <- tryCatch({ con <- url(u, headers = c("User-Agent" = UA)); on.exit(close(con), add = TRUE)
                  jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "")) },
                error = function(e) NULL)
  Sys.sleep(1.1)                                 # Usage policy: 1 request per second
  if (is.null(r) || length(r) == 0 || (is.data.frame(r) && nrow(r) == 0)) return(NULL)
  list(lat = as.numeric(r$lat[1]), lon = as.numeric(r$lon[1]), nom = as.character(r$display_name[1]))
}

# 5. Resumable run ----
if (fs::file_exists(OUT_GEO)) {
  hecho <- fread(OUT_GEO, encoding = "UTF-8")
  pend  <- est[!nro_inscripcion %in% hecho$nro_inscripcion]
  cat("Resuming: done", nrow(hecho), "| pending", nrow(pend), "\n")
} else {
  hecho <- NULL; pend <- est
  cat("New run:", nrow(pend), "stations\n")
}
cat("Stations without a department (cannot be geocoded):", sum(is.na(pend$departamento)), "\n")

# Shuffle the order. The panel is sorted by province, so the match rates printed
# during a partial run would be biased otherwise. The seed keeps it reproducible.
set.seed(1); pend <- pend[sample(.N)]

# Test mode: with the environment variable GEO_N_TEST=30 only 30 stations are
# processed, to check that everything works before the long run.
.n_test <- suppressWarnings(as.integer(Sys.getenv("GEO_N_TEST", "0")))
if (!is.na(.n_test) && .n_test > 0) {
  pend <- head(pend, .n_test)
  cat("Test mode: only", nrow(pend), "stations\n")
}

acum <- vector("list", nrow(pend)); n_ok <- 0L
for (i in seq_len(nrow(pend))) {
  r <- pend[i]; d <- limpia_dir(r$direccion)
  fuente <- "centroide_depto"; lat <- r$clat; lon <- r$clon; det <- NA_character_
  # Highway addresses skip georef and go straight to Nominatim
  z <- if (!r$ruta && !is.na(r$prov_id)) geo_georef(d, r$prov_id, r$departamento, r$localidad) else NULL
  if (!is.null(z)) { fuente <- "georef"; lat <- z$lat; lon <- z$lon; det <- z$dep }
  else {
    q <- paste(d, r$localidad, r$provincia, "Argentina", sep = ", ")
    z2 <- geo_nominatim(q, r$clat, r$clon)
    if (!is.null(z2)) { fuente <- "nominatim"; lat <- z2$lat; lon <- z2$lon; det <- substr(z2$nom, 1, 90) }
  }
  acum[[i]] <- data.table(
    nro_inscripcion = r$nro_inscripcion, provincia = r$provincia, departamento = r$departamento,
    localidad = r$localidad, direccion = r$direccion, tipo = ifelse(r$ruta, "ruta", "urbana"),
    lat = lat, lon = lon,
    fuente = ifelse(fuente == "centroide_depto" & is.na(lat), "sin_dato", fuente),
    precision = ifelse(fuente != "centroide_depto", "exacta",
                       ifelse(is.na(lat), "sin_dato", "aproximada")),
    km_al_centroide = ifelse(is.na(lat) | is.na(r$clat), NA_real_,
                             round(km_ent(lat, lon, r$clat, r$clon), 1)),
    detalle = ifelse(is.na(det), "", det))
  if (fuente != "centroide_depto") n_ok <- n_ok + 1L
  if (i %% 50 == 0 || i == nrow(pend)) {                       # Save every 50 stations
    out <- rbindlist(c(list(hecho), acum[seq_len(i)]), use.names = TRUE, fill = TRUE)
    fwrite(out, OUT_GEO, na = "NA", bom = TRUE)
    cat(sprintf("  %5d/%d  exact so far: %d (%.0f%%)\n", i, nrow(pend), n_ok, 100*n_ok/i))
  }
}

cat("\nDone\n")
fin <- fread(OUT_GEO, encoding = "UTF-8")
print(fin[, .N, by = .(tipo, fuente)][order(tipo, -N)])
cat("\nExact:", sum(fin$precision == "exacta"), "/", nrow(fin),
    sprintf(" (%.1f%%)\n", 100*mean(fin$precision == "exacta")))
cat("File:", as.character(OUT_GEO), "\n")

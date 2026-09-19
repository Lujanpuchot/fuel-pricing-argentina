# 02_geocode_structured_search.R
# Second geocoding pass: the stations that 01_geocode_addresses.R left as
# "aproximada" are tried again with the structured search of Nominatim.
#
# Input:  geocodificacion_estaciones.csv (not modified), centroides_departamento.csv
# Output: geocodificacion_pass2.csv (rescued stations only)
#
# The structured search takes street and number, city, county (the department) and
# state (the province) as separate fields.
#
# Do not run at the same time as 01_geocode_addresses.R. Nominatim allows 1 request
# per second in total, and two processes risk getting the IP address blocked.
#
# With the environment variable GEO_PARSE_ONLY=1 the script makes no requests and
# only prints how a sample of addresses is split into street and number.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(fs)
})
source("code/00_config.R")
options(timeout = 60)

DIR_INT <- DIR_INTERIM
IN_GEO  <- fs::path(DIR_INT, "geocodificacion_estaciones.csv")
OUT_P2  <- fs::path(DIR_INT, "geocodificacion_pass2.csv")
CENT    <- fs::path(DIR_INT, "centroides_departamento.csv")
UA <- sprintf("fuel-retail-geocoder/1.0 (academic research; %s)", CONTACT_EMAIL)
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

# Address parsing ----

limpia_dir <- function(x){
  s0 <- as.character(x)
  # Some addresses come glued together ("RutaProvincial4Kilometro4"): always split
  # camelCase, then split letter-digit boundaries only if there is still no space
  s0 <- gsub("([a-z])([A-Z])", "\\1 \\2", s0)
  if (!grepl(" ", trimws(s0))) {
    s0 <- gsub("([A-Za-z])([0-9])", "\\1 \\2", s0)
    s0 <- gsub("([0-9])([A-Za-z])", "\\1 \\2", s0)
  }
  # Remove "Nº", "N°" and "Nro" before transliterating. Otherwise iconv turns "º"
  # into "o", the pattern no longer matches and "NO 799" stays glued to the street name.
  s0 <- gsub("Â", "", s0)                                  # Mojibake in the source data
  s0 <- gsub("N[º°]", " ", s0)
  s0 <- gsub("\\bNro\\.?\\b", " ", s0, ignore.case = TRUE)
  s <- norm(s0)
  s <- gsub("\\(.*?\\)", " ", s); s <- gsub("\\s+-\\s+[A-Z ]+$", " ", s)
  s <- gsub("\\bAVDA\\.?\\b|\\bAV\\.", "AV ", s); s <- gsub("\\bGRAL\\.?\\b", "GENERAL ", s)
  s <- gsub("\\bBV\\.?\\b|\\bBVAR\\.?\\b", "BOULEVARD ", s)
  s <- gsub("\\bESQ\\.?\\b|\\bESQUINA\\b", " Y ", s); s <- gsub("S/N[ºO]?|SIN NUMERO", " ", s)
  s <- gsub("[^A-Z0-9 ]", " ", s)
  gsub("\\s+", " ", trimws(s))
}

# Flags street corners, which the structured search cannot resolve
es_interseccion <- function(s) grepl(" Y | E ", s)

# Split an address into calle (street) and altura (house number). In La Plata and a
# few other cities the street name is itself a number ("CALLE 66", "AV 520"): if
# nothing but a generic word is left after removing the number, the address is not
# split and the whole string is the street name.
GENERICOS <- c("", "AV", "AVENIDA", "CALLE", "CALLES", "BOULEVARD", "DIAGONAL", "RUTA")
parse_calle_altura <- function(d){
  s <- limpia_dir(d)
  # For corners keep the first street, which usually carries the number
  if (es_interseccion(s)) s <- trimws(strsplit(s, " Y | E ")[[1]][1])
  # A trailing compass word hides the number ("... 1316 OESTE"), so drop it
  s <- trimws(gsub("\\s+(OESTE|ESTE|NORTE|SUR)\\s*$", "", s))
  m <- regmatches(s, regexpr("[0-9]{1,6}\\s*$", s))
  if (length(m) == 1L && nzchar(m)) {
    altura <- trimws(m)
    resto  <- trimws(sub("[0-9]{1,6}\\s*$", "", s))
    if (norm(resto) %in% GENERICOS) return(list(calle = s, altura = ""))  # "AV 520": the number is the name
    return(list(calle = resto, altura = altura))
  }
  list(calle = s, altura = "")
}

# Offline test mode ----
if (nzchar(Sys.getenv("GEO_PARSE_ONLY"))) {
  g <- fread(IN_GEO, encoding = "UTF-8")
  pend <- g[precision == "aproximada" & tipo == "urbana"]
  set.seed(5); pend <- pend[sample(.N, min(18, .N))]
  cat("How addresses are split (no API calls)\n")
  pr <- rbindlist(lapply(seq_len(nrow(pend)), function(i){
    z <- parse_calle_altura(pend$direccion[i])
    data.table(original = substr(pend$direccion[i], 1, 34),
               calle = substr(z$calle, 1, 26), altura = z$altura)
  }))
  print(pr, nrows = 30); quit(status = 0)
}

# Structured query ----
nomi_estructurado <- function(calle, altura, ciudad, depto, prov, clat, clon, d = 0.45){
  street <- trimws(paste(altura, calle))          # Nominatim expects "<number> <street>"
  p <- c(sprintf("street=%s", utils::URLencode(street, reserved = TRUE)),
         sprintf("city=%s",   utils::URLencode(as.character(ciudad), reserved = TRUE)),
         sprintf("state=%s",  utils::URLencode(as.character(prov), reserved = TRUE)),
         "country=Argentina", "format=json", "limit=1")
  if (!is.na(depto) && nzchar(as.character(depto)))
    p <- c(p, sprintf("county=%s", utils::URLencode(as.character(depto), reserved = TRUE)))
  if (!is.na(clat))
    p <- c(p, sprintf("viewbox=%f,%f,%f,%f", clon-d, clat+d, clon+d, clat-d), "bounded=1")
  u <- paste0("https://nominatim.openstreetmap.org/search?", paste(p, collapse = "&"))
  r <- tryCatch({ con <- url(u, headers = c("User-Agent" = UA)); on.exit(close(con), add = TRUE)
                  jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "")) },
                error = function(e) NULL)
  Sys.sleep(1.1)                                  # Usage policy: 1 request per second
  if (is.null(r) || length(r) == 0 || (is.data.frame(r) && nrow(r) == 0)) return(NULL)
  list(lat = as.numeric(r$lat[1]), lon = as.numeric(r$lon[1]), nom = as.character(r$display_name[1]))
}

# Rescue run ----
g <- fread(IN_GEO, encoding = "UTF-8")
cent <- fread(CENT, encoding = "UTF-8")
g <- merge(g, cent, by = c("provincia","departamento"), all.x = TRUE)
# Only urban addresses left as approximate. The structured search has a single
# street field and cannot resolve highway-plus-km addresses (about 30 minutes of
# certain failures if tried). Corners are sent with their first street only.
pend <- g[precision == "aproximada" & tipo == "urbana"]
cat("Rescue candidates (urban, approximate):", nrow(pend), "\n")
# Only rescued stations are saved, so a resumed run queries earlier failures again
if (fs::file_exists(OUT_P2)) {
  ya <- fread(OUT_P2, encoding = "UTF-8")
  pend <- pend[!nro_inscripcion %in% ya$nro_inscripcion]
  cat("Resuming: pending", nrow(pend), "\n")
} else ya <- NULL
# Network test: GEO_N_TEST=15 runs only 15 random stations, before the long run
.n_test <- suppressWarnings(as.integer(Sys.getenv("GEO_N_TEST", "0")))
if (!is.na(.n_test) && .n_test > 0) {
  set.seed(9); pend <- pend[sample(.N, min(.n_test, .N))]
  cat("Test mode:", nrow(pend), "stations\n")
}

acum <- vector("list", nrow(pend)); n_ok <- 0L
for (i in seq_len(nrow(pend))) {
  r <- pend[i]; z <- parse_calle_altura(r$direccion)
  h <- nomi_estructurado(z$calle, z$altura, r$localidad, r$departamento, r$provincia, r$clat, r$clon)
  if (!is.null(h)) {
    n_ok <- n_ok + 1L
    km <- if (is.na(r$clat)) NA_real_ else
      round(111*sqrt((h$lat-r$clat)^2 + ((h$lon-r$clon)*cos(h$lat*pi/180))^2), 1)
    acum[[i]] <- data.table(nro_inscripcion = r$nro_inscripcion, lat = h$lat, lon = h$lon,
                            fuente = "nominatim_estructurado", precision = "exacta",
                            km_al_centroide = km, detalle = substr(h$nom, 1, 90))
  }
  if (i %% 50 == 0 || i == nrow(pend)) {
    out <- rbindlist(c(list(ya), acum[seq_len(i)]), use.names = TRUE, fill = TRUE)
    if (nrow(out)) fwrite(out, OUT_P2, na = "NA", bom = TRUE)
    cat(sprintf("  %5d/%d  rescued: %d (%.0f%%)\n", i, nrow(pend), n_ok, 100*n_ok/i))
  }
}
cat("\nSecond pass done. Rescued:", n_ok, "of", nrow(pend), "\n")

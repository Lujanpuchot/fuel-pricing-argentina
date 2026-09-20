# 07_geocode_stations.R
# Coordinates for the outlets that sell to the public. The source has addresses
# but no coordinates, so each part adds a layer over the outlets the previous
# parts left unresolved, and every coordinate carries a precision label
# (exact, locality or department).
#
# Input:  the analysis panel with the department crosswalk
# Output: geocodificacion_final.csv in DIR_INTERIM, plus the file each pass
#         writes, which is reused when the script runs again
#
# Parts 1 to 4 query public services one address at a time. Nominatim allows
# one request per second, so a full run takes hours. Set FUEL_CONTACT_EMAIL so
# that the requests identify you, as its usage policy asks.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
  library(jsonlite)
  library(sf)          # section 3, distance to the nearest refinery
})

source("code/00_config.R")

DIR_INT <- DIR_INTERIM
DIR     <- DIR_INTERIM

UA <- sprintf("fuel-retail-geocoder/1.0 (academic research; %s)", CONTACT_EMAIL)

# 1. Street address: georef and Nominatim ----

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

options(timeout = 60)

FILE_IN  <- fs::path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
OUT_GEO  <- fs::path(DIR_INT, "geocodificacion_estaciones.csv")
OUT_CENT <- fs::path(DIR_INT, "centroides_departamento.csv")

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

# 1.1 Unique stations ----
# One address per station: the first one that appears in the panel
b <- readRDS(FILE_IN); setDT(b)
est <- unique(b[canal_de_comercializacion == "Al público",
                .(nro_inscripcion, direccion, localidad, provincia, departamento)],
              by = "nro_inscripcion")
est <- est[!is.na(direccion) & trimws(direccion) != ""]
rm(b); invisible(gc())
est[, ruta := grepl("\\bRUTA\\b|\\bRN\\b|\\bRP\\b|\\bAUTOPISTA\\b|\\bAUTOVIA\\b|\\bKM\\b", norm(direccion))]
cat("Stations to geocode:", nrow(est), "| on highways:", sum(est$ruta), "\n")

# 1.2 Province ids ----
pv <- GJ("https://apis.datos.gob.ar/georef/api/provincias?max=30")$provincias
p_id <- as.character(pv$id); p_nom <- norm(pv$nombre)
mapid <- setNames(p_id, p_nom)
# The panel names these two provinces differently from georef
mapid[["CAPITAL FEDERAL"]]  <- p_id[grepl("AUTONOMA", p_nom)][1]
mapid[["TIERRA DEL FUEGO"]] <- p_id[grepl("TIERRA DEL FUEGO", p_nom)][1]
est[, prov_id := mapid[norm(provincia)]]

# 1.3 Department centroids (cached) ----
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

# 1.4 Geocoders ----

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

# 1.5 Resumable run ----
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

# 2. Urban addresses: structured search ----

# Second geocoding pass: the stations that section 1 left as
# "aproximada" are tried again with the structured search of Nominatim.
#
# Input:  geocodificacion_estaciones.csv (not modified), centroides_departamento.csv
# Output: geocodificacion_pass2.csv (rescued stations only)
#
# The structured search takes street and number, city, county (the department) and
# state (the province) as separate fields.
#
# Do not run two geocoding passes at once. Nominatim allows 1 request
# per second in total, and two processes risk getting the IP address blocked.
#
# With the environment variable GEO_PARSE_ONLY=1 the script makes no requests and
# only prints how a sample of addresses is split into street and number.

options(timeout = 60)

IN_GEO  <- fs::path(DIR_INT, "geocodificacion_estaciones.csv")
OUT_P2  <- fs::path(DIR_INT, "geocodificacion_pass2.csv")
CENT    <- fs::path(DIR_INT, "centroides_departamento.csv")
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
  print(pr, nrows = 30)
  quit(status = 0)   # test mode ends the session: run this script on its own, not through run_all.R
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

# 3. Locality centroids and consolidation ----

# Locality-centroid tier of the geocoding, and consolidation of all tiers into one
# file with a graded precision column.
#
# Input:  geocodificacion_estaciones.csv, geocodificacion_pass2.csv
# Output: geocodificacion_final.csv         (consolidated, 8,720 rows)
#         centroides_localidad.csv          (locality centroids, reused as a cache)
#         variables_espaciales_estacion.csv (distance to the nearest refinery)
# The station-level files merge on nro_inscripcion.
#
# Stations with no address-level match were sitting on the centroid of their
# department, which is very coarse. Here they move to the centroid of their
# localidad (locality), that is, the town center. This is far more precise, above
# all away from the big cities, where towns are small and the center is roughly
# where the station is.
#
# Levels of the precision column in the consolidated file:
#   exacta        geocoded address (first and second pass); good for every variable
#   localidad     town center; fine for distance to a refinery or for regions, not
#                 for nearest-rival measures, because all the stations of a town
#                 share the same point
#   departamento  department centroid, last resort
#   sin_dato      neither locality nor department
#
# The station-year panel of nearby rivals (variables_espaciales_panel.csv) is not
# rebuilt here and stays on the exact subset. Section 1 of 08_station_variables.R
# builds it, and writes variables_espaciales_estacion.csv again from the final
# coordinates, so that copy is the one to use.
# Sections 4 to 6 below edit geocodificacion_final.csv in place.

sf::sf_use_s2(TRUE)
options(timeout = 60)

GEO1  <- file.path(DIR, "geocodificacion_estaciones.csv")
GEO2  <- file.path(DIR, "geocodificacion_pass2.csv")
CENTL <- file.path(DIR, "centroides_localidad.csv")
FINAL <- file.path(DIR, "geocodificacion_final.csv")
OUT_E <- file.path(DIR, "variables_espaciales_estacion.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))
GJ   <- function(u) tryCatch(jsonlite::fromJSON(u), error = function(e) NULL)

# 3.1 Current state: first pass plus the second-pass rescues ----
g <- fread(GEO1, encoding = "UTF-8")
if (file.exists(GEO2)) {
  p2 <- fread(GEO2, encoding = "UTF-8")
  if (nrow(p2)) {
    g <- merge(g, p2[, .(nro_inscripcion, lat2 = lat, lon2 = lon, f2 = fuente)], by = "nro_inscripcion", all.x = TRUE)
    g[!is.na(lat2), `:=`(lat = lat2, lon = lon2, fuente = f2, precision = "exacta")]
    g[, c("lat2","lon2","f2") := NULL]
  }
}
cat("Initial state, exact:", sum(g$precision == "exacta"), "| to improve:", sum(g$precision != "exacta"), "\n")

# 3.2 Province ids (georef) ----
pv <- GJ("https://apis.datos.gob.ar/georef/api/provincias?max=30")$provincias
p_id <- as.character(pv$id); p_nom <- norm(pv$nombre); mapid <- setNames(p_id, p_nom)
mapid[["CAPITAL FEDERAL"]]  <- p_id[grepl("AUTONOMA", p_nom)][1]
mapid[["TIERRA DEL FUEGO"]] <- p_id[grepl("TIERRA DEL FUEGO", p_nom)][1]

# 3.3 Locality centroids (cached, resumable) ----
noex <- g[precision != "exacta" & !is.na(localidad) & trimws(localidad) != ""]
locs <- unique(noex[, .(provincia, localidad)])
locs[, prov_id := mapid[norm(provincia)]]
if (file.exists(CENTL)) {
  cl <- fread(CENTL, encoding = "UTF-8")
  locs <- locs[!paste(provincia,localidad) %in% paste(cl$provincia, cl$localidad)]
  cat("Cached localities:", nrow(cl), "| to download:", nrow(locs), "\n")
} else cl <- NULL
if (nrow(locs)) {
  nue <- rbindlist(lapply(seq_len(nrow(locs)), function(i){
    la <- lo <- NA_real_
    if (!is.na(locs$prov_id[i])) {
      u <- sprintf("https://apis.datos.gob.ar/georef/api/localidades?nombre=%s&provincia=%s&campos=centroide&max=1",
                   utils::URLencode(as.character(locs$localidad[i]), reserved = TRUE), locs$prov_id[i])
      r <- GJ(u); Sys.sleep(0.12)
      if (!is.null(r) && length(r$localidades) > 0 && nrow(r$localidades) > 0) {
        la <- tryCatch(as.numeric(r$localidades$centroide$lat[1]), error = function(e) NA_real_)
        lo <- tryCatch(as.numeric(r$localidades$centroide$lon[1]), error = function(e) NA_real_)
      }
    }
    if (i %% 100 == 0) cat("  ", i, "/", nrow(locs), "\n")
    data.table(provincia = locs$provincia[i], localidad = locs$localidad[i], llat = la, llon = lo)
  }))
  cl <- rbindlist(list(cl, nue), use.names = TRUE, fill = TRUE)
  fwrite(cl, CENTL, na = "NA", bom = TRUE)
}
cat("Locality centroids found:", sum(!is.na(cl$llat)), "/", nrow(cl), "\n")

# 3.4 Graded precision and consolidated file ----
g <- merge(g, cl[, .(provincia, localidad, llat, llon)], by = c("provincia","localidad"), all.x = TRUE)
up <- g$precision != "exacta" & !is.na(g$llat)                     # Move up to the locality centroid
g[up, `:=`(lat = llat, lon = llon, fuente = "centroide_localidad", precision = "localidad")]
g[precision == "aproximada", precision := "departamento"]          # The ones that stay on the department centroid
g[is.na(lat), precision := "sin_dato"]
g[, c("llat","llon") := NULL]
fwrite(g, FINAL, na = "NA", bom = TRUE)

cat("\nConsolidated file: geocodificacion_final.csv\n")
print(g[, .N, by = precision][order(-N)])
cat(sprintf("Located (exact or locality): %d (%.1f%%)\n",
            sum(g$precision %in% c("exacta","localidad")),
            100*mean(g$precision %in% c("exacta","localidad"))))

# 3.5 Distance to the nearest refinery (exact and locality levels) ----
# Refinery coordinates are approximate (two decimals).
REF <- data.table(
  refineria = c("La Plata (YPF)","Luján de Cuyo (YPF)","Plaza Huincul (YPF)","Dock Sud (Raízen/Shell)",
                "Campana (Axion/PAE)","Bahía Blanca (Trafigura/Puma)","Campo Durán (Refinor)"),
  lat = c(-34.86,-33.03,-38.93,-34.65,-34.17,-38.75,-22.20),
  lon = c(-57.90,-68.88,-69.20,-58.34,-58.96,-62.27,-63.70))
ub <- g[precision %in% c("exacta","localidad") & !is.na(lat)]
pe <- st_as_sf(ub, coords = c("lon","lat"), crs = 4326)
pr <- st_as_sf(REF, coords = c("lon","lat"), crs = 4326)
idx <- st_nearest_feature(pe, pr)
ub[, `:=`(refineria_cercana = REF$refineria[idx],
          d_refineria_km = round(as.numeric(st_distance(pe, pr[idx,], by_element = TRUE))/1000, 1))]
fwrite(ub[, .(nro_inscripcion, precision, refineria_cercana, d_refineria_km)], OUT_E, na = "NA", bom = TRUE)
cat("\nDistance to refinery recomputed for", nrow(ub), "located stations (exact or locality)\n")

# 4. Audit of exact matches ----

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
# Urban addresses only; highway addresses are handled in section 5.
#
# Test run on 8 stations, which leaves the final file untouched:
#   GEO_N_TEST=8 Rscript code/07_geocode_stations.R   (stops after this section)

options(timeout = 60)

FIN   <- fs::path(DIR_INT, "geocodificacion_final.csv")
CENTL <- fs::path(DIR_INT, "centroides_localidad.csv")
CACHE <- fs::path(DIR_INT, "fix_cola_cache.csv")
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

# Address parser (same as in section 2) ----
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
  quit(status = 0)   # as above: test mode ends the session
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

# 5. Highway addresses: kilometre posts ----

# Locate "RUTA X km Y" addresses with the kilometer posts published by the national
# highway agency (Dirección Nacional de Vialidad, DNV).
#
# Input:  geocodificacion_final.csv, postes_km_dnv.csv
# Output: geocodificacion_final.csv (overwritten in place)
#
# Highway addresses with a kilometer were the largest group without an exact
# location: the km is not a house number and no free geocoder resolves it. The DNV
# posts give the coordinates of every km of the national highways (field
# progresiva), so "RN 3 km 1845" is placed by interpolating between posts.
#
# Source: DNV / Secretaría de Transporte, open data, 2018-19 survey, layer
# poste_1km_2018 of https://datos.transporte.gob.ar/dataset/postes-kilometricos.
# Stored trimmed as postes_km_dnv.csv (cod_ruta, ruta_num, progresiva, lat, lon).
#
# Limitations:
#   - National highways only. Provincial routes are not in the source, so they are
#     not resolved here.
#   - The posts date from 2018-19: newer bypasses and detours can shift some km by
#     a few km. Still far better than the town center.
#   - Precision of about 1-2 km (one post per km or so). These stations get
#     fuente = "poste_km_dnv" and keep tipo = "ruta", so the analysis can tell them
#     apart from urban addresses.
# Only stations without an exact location are touched.

FIN    <- file.path(DIR_INT, "geocodificacion_final.csv")
POSTES <- file.path(DIR_INT, "postes_km_dnv.csv")
if (!file.exists(POSTES)) POSTES <- file.path("data", "postes_km_dnv.csv")   # copy in the repository
GATE_KM <- 80   # The new point must fall within 80 km of the current location (catches misparsed routes)

norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))
haversine <- function(lat1, lon1, lat2, lon2){
  R<-6371; p<-pi/180
  a <- sin((lat2-lat1)*p/2)^2 + cos(lat1*p)*cos(lat2*p)*sin((lon2-lon1)*p/2)^2
  2*R*asin(pmin(1,sqrt(a)))
}

# Route number and km from the text of the address
parse_rutakm <- function(d){
  s <- norm(d)
  s <- gsub("\\bRUTA NACIONAL\\b","RN ",s); s <- gsub("\\bRUTA PROVINCIAL\\b","RP ",s)
  pre <- gsub("KM.*$","",s)
  m <- regmatches(pre, regexpr("(RUTA|RN|RP|R N|R P)\\D{0,4}[0-9]{1,3}", pre))
  num <- if(length(m)) as.integer(regmatches(m, regexpr("[0-9]{1,3}$", m))) else NA_integer_
  mk <- regmatches(s, regexpr("KM\\.?\\s*[0-9]{1,4}([.,][0-9]+)?", s))
  km <- if(length(mk)) as.numeric(gsub(",",".",regmatches(mk, regexpr("[0-9]{1,4}([.,][0-9]+)?$", mk)))) else NA_real_
  list(num=num, km=km)
}

g <- fread(FIN, encoding="UTF-8")
P <- fread(POSTES, encoding="UTF-8"); setorder(P, ruta_num, progresiva)
Plist <- split(P, P$ruta_num)   # Posts by highway, for fast lookup

# Interpolate lat/lon at the requested km along the posts of the highway
place_on_route <- function(num, km){
  pr <- Plist[[as.character(num)]]
  if (is.null(pr) || !nrow(pr)) return(NULL)
  below <- pr[progresiva <= km]; above <- pr[progresiva >= km]
  if (nrow(below) && nrow(above)) {
    b <- below[.N]; a <- above[1]
    gap <- a$progresiva - b$progresiva
    if (gap == 0) return(list(lat=b$lat, lon=b$lon, gap=0))
    if (gap > 25)  {                                   # Large gap between posts: use the nearest one if within 5 km
      near <- pr[which.min(abs(progresiva - km))]
      if (abs(near$progresiva - km) <= 5) return(list(lat=near$lat, lon=near$lon, gap=abs(near$progresiva-km)))
      return(NULL)
    }
    f <- (km - b$progresiva) / gap                     # Linear interpolation in km
    return(list(lat = b$lat + f*(a$lat-b$lat), lon = b$lon + f*(a$lon-b$lon), gap=gap))
  }
  near <- pr[which.min(abs(progresiva - km))]          # Km beyond the range of the posts: nearest one if within 5 km
  if (abs(near$progresiva - km) <= 5) return(list(lat=near$lat, lon=near$lon, gap=abs(near$progresiva-km)))
  NULL
}

# Targets: highway stations without an exact location, with a route number and a
# km, on a route number that appears in the DNV posts
tgt <- g[tipo=="ruta" & precision!="exacta"]
pk <- rbindlist(lapply(tgt$direccion, function(d){z<-parse_rutakm(d); data.table(num=z$num, km=z$km)}))
tgt <- cbind(tgt, pk)
tgt <- tgt[!is.na(num) & !is.na(km) & num %in% P$ruta_num]
cat("Candidates (highway, not exact, route number and km, national highway):", nrow(tgt), "\n")

acc <- 0L; rej_gate <- 0L; fail <- 0L; upd <- list()
for (i in seq_len(nrow(tgt))) {
  r <- tgt[i]; z <- place_on_route(r$num, r$km)
  if (is.null(z)) { fail <- fail + 1L; next }
  dmove <- if (is.na(r$lat)) 0 else haversine(z$lat, z$lon, r$lat, r$lon)   # Distance from the current location
  if (!is.na(r$lat) && dmove > GATE_KM) { rej_gate <- rej_gate + 1L; next }
  acc <- acc + 1L
  upd[[length(upd)+1]] <- data.table(nro_inscripcion=r$nro_inscripcion, lat=z$lat, lon=z$lon,
                                     detalle=sprintf("RN%d km%g (poste DNV, gap %.0f km)", r$num, r$km, z$gap))
}
cat(sprintf("Accepted: %d | rejected by the distance check (>%d km): %d | no post nearby: %d\n",
            acc, GATE_KM, rej_gate, fail))

if (acc) {
  U <- rbindlist(upd); setkey(g, nro_inscripcion)
  for (i in seq_len(nrow(U))) {
    ni <- U$nro_inscripcion[i]
    g[.(ni), `:=`(lat=U$lat[i], lon=U$lon[i], fuente="poste_km_dnv",
                  precision="exacta", km_al_centroide=NA_real_, detalle=U$detalle[i])]
  }
}
cat("\nNew breakdown by precision\n"); print(g[, .N, by=precision][order(-N)])
cat("Exact:", g[precision=="exacta",.N], sprintf("(%.1f%%)\n", 100*mean(g$precision=="exacta")))

cols <- c("provincia","localidad","nro_inscripcion","departamento","direccion","tipo",
          "lat","lon","fuente","precision","km_al_centroide","detalle")
fwrite(g[, ..cols], FIN, na="NA", bom=TRUE)
cat("geocodificacion_final.csv overwritten\n")

# 6. Official coordinates ----

# Overlay the official coordinates of the Energy Secretariat (Secretaría de
# Energía) as the last layer of the geocoding.
#
# Input:  geocodificacion_final.csv, coords_oficiales_energia.csv
# Output: geocodificacion_final.csv (overwritten in place)
#
# The public data set of pump prices (Resolución 314/2016) carries the latitude and
# longitude that each operator registered for its outlets. Its idempresa is the
# same id as nro_inscripcion, so the coordinates merge directly, with no geocoding.
# Where both exist they agree closely with the geocoded points (median distance of
# about 78 m).
#
# Source: http://datos.energia.gob.ar/dataset/precios-en-surtidor, resource
# "Precios históricos" (idempresa, latitud, longitud), collapsed to one coordinate
# per station (the median) in coords_oficiales_energia.csv.
#
# Conservative rule: the official coordinates fill only the stations that the
# cascade left without an exact location (localidad, departamento or sin_dato). The
# exact matches, already audited, stay as they are; they are about 78 m from the
# official point anyway. Filled rows get fuente = "energia_oficial" and
# precision = "exacta".

FIN <- file.path(DIR_INT, "geocodificacion_final.csv")
CO  <- file.path(DIR_INT, "coords_oficiales_energia.csv")
if (!file.exists(CO)) CO <- file.path("data", "coords_oficiales_energia.csv")   # copy in the repository
haversine <- function(lat1, lon1, lat2, lon2){
  R<-6371; p<-pi/180
  a <- sin((lat2-lat1)*p/2)^2 + cos(lat1*p)*cos(lat2*p)*sin((lon2-lon1)*p/2)^2
  2*R*asin(pmin(1,sqrt(a)))
}

g  <- fread(FIN, encoding="UTF-8")
co <- fread(CO,  encoding="UTF-8")[!is.na(olat)]
cat("Official coordinates available:", nrow(co), "stations\n")

# For information only: distance between the geocoded and the official point for
# the stations that already have an exact match
v <- merge(g[precision=="exacta", .(nro_inscripcion, lat, lon)],
           co[, .(nro_inscripcion=idempresa, olat, olon)], by="nro_inscripcion")
if (nrow(v)) {
  v[, d := haversine(lat,lon,olat,olon)]
  cat(sprintf("Exact geocoded vs official (n=%d): median %.0f m | p90 %.2f km | >5 km: %.1f%%\n",
      nrow(v), 1000*median(v$d), quantile(v$d,.9), 100*mean(v$d>5)))
}

# Fill only the stations without an exact location that have an official
# coordinate; exact matches are not touched
antes <- g[, .N, by=precision][order(-N)]
g <- merge(g, co[, .(nro_inscripcion=idempresa, olat, olon)], by="nro_inscripcion", all.x=TRUE)
fill <- !is.na(g$olat) & g$precision != "exacta"
g[fill, `:=`(lat=olat, lon=olon, fuente="energia_oficial", precision="exacta",
             km_al_centroide=NA_real_, detalle="coord oficial Energia (Res 314/2016)")]
g[, c("olat","olon") := NULL]
cat("\nNon-exact stations filled with official coordinates:", sum(fill), "\n")
# In the table N is the count before and despues the count after
cat("\nBreakdown by precision, before and after\n")
print(merge(antes, g[, .(despues=.N), by=precision], by="precision", all=TRUE)[order(-despues)])
cat("Exact:", g[precision=="exacta",.N], sprintf("(%.1f%%)\n", 100*mean(g$precision=="exacta")))
cat("Located (exact or locality):", g[precision %in% c("exacta","localidad"),.N],
    sprintf("(%.1f%%)\n", 100*mean(g$precision %in% c("exacta","localidad"))))

cols <- c("provincia","localidad","nro_inscripcion","departamento","direccion","tipo",
          "lat","lon","fuente","precision","km_al_centroide","detalle")
fwrite(g[, ..cols], FIN, na="NA", bom=TRUE)
cat("geocodificacion_final.csv overwritten\n")


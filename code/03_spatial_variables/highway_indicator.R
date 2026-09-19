# highway_indicator.R
# Geometric indicator of whether a station is on a highway, from its distance to
# the OpenStreetMap trunk road network.
#
# Input:  geocodificacion_final.csv
#         raw_osm/osm_rutas_troncales.json (plus osm_rutas_secundarias.json if present)
# Output: variables_ruta_estacion.csv
#           d_ruta_km    km from the station to the centerline of the nearest trunk road
#           clase_ruta   OSM class of that road: motorway / trunk / primary
#           sobre_ruta   1 if d_ruta_km <= 0.20 (about the margin of geocoding error)
#
# The geocoding file already has `tipo` (urbana/ruta), derived from the text of
# the address; the geometric version is a better product characteristic for the
# demand model. The script ends with a cross-tabulation of the two flags.
#
# Road network: OSM ways of Argentina with highway in {motorway, trunk, primary}
# (roughly freeways, national routes and the main provincial routes), downloaded
# through Overpass and cached in raw_osm/osm_rutas_troncales.json (59 MB).
# Secondary roads were not downloaded because of the Overpass rate limit; they
# are added only if osm_rutas_secundarias.json exists and is a complete download.
#
# Only stations with exact coordinates are used: the locality centroid would bias
# the distance downward, because towns sit on the highway.

suppressPackageStartupMessages({library(data.table); library(jsonlite); library(sf)})
source("code/00_config.R")
sf_use_s2(TRUE)
INT  <- DIR_INTERIM
CACHE<- file.path(INT, "raw_osm")   # OSM geometry, kept with the rest of the data

# 1. Trunk road network from the Overpass JSON ----
# Returns NULL if the file is missing or smaller than min_mb (not a complete download)
leer_red <- function(f, min_mb = 5) {
  if (!file.exists(f) || file.size(f) < min_mb*1e6) return(NULL)
  j <- fromJSON(f, simplifyVector = FALSE)
  els <- j$elements
  cat(basename(f), ":", length(els), "ways\n")
  lns <- vector("list", length(els)); cls <- character(length(els))
  for (i in seq_along(els)) {
    g <- els[[i]]$geometry
    m <- matrix(c(vapply(g, `[[`, 0, "lon"), vapply(g, `[[`, 0, "lat")), ncol=2)
    lns[[i]] <- st_linestring(m)
    cls[i]   <- els[[i]]$tags$highway %||% "?"
  }
  st_sf(clase = cls, geometry = st_sfc(lns, crs = 4326))
}
`%||%` <- function(a,b) if (is.null(a)) b else a
red <- leer_red(file.path(CACHE, "osm_rutas_troncales.json"))
sec <- leer_red(file.path(CACHE, "osm_rutas_secundarias.json"))
if (!is.null(sec)) red <- rbind(red, sec)
stopifnot(!is.null(red))
cat("full network:", nrow(red), "segments | classes:", paste(names(table(red$clase)), collapse="/"), "\n")

# 2. Stations with exact coordinates ----
g <- fread(file.path(INT, "geocodificacion_final.csv"), encoding = "UTF-8")
ex <- g[precision == "exacta" & !is.na(lat)]
pe <- st_as_sf(ex, coords = c("lon","lat"), crs = 4326)

# 3. Distance to the nearest road centerline ----
# Nearest road from the spatial index, then a single distance per station
idx <- st_nearest_feature(pe, red)
d   <- as.numeric(st_distance(pe, red[idx,], by_element = TRUE)) / 1000
out <- data.table(nro_inscripcion = ex$nro_inscripcion,
                  d_ruta_km   = round(d, 3),
                  clase_ruta  = red$clase[idx],
                  sobre_ruta  = as.integer(d <= 0.20),
                  tipo_texto  = ex$tipo)
fwrite(out[, .(nro_inscripcion, d_ruta_km, clase_ruta, sobre_ruta)],
       file.path(INT, "variables_ruta_estacion.csv"), na = "NA", bom = TRUE)

# 4. Summary and cross-check against the text-based flag ----
cat("\n[variables_ruta_estacion.csv]", nrow(out), "stations (exact coordinates only)\n")
cat("\nd_ruta_km:  median", round(median(out$d_ruta_km),3),
    "| p75", round(quantile(out$d_ruta_km,.75),2), "| p95", round(quantile(out$d_ruta_km,.95),1), "km\n")
cat("sobre_ruta (<=200 m):", sum(out$sobre_ruta), sprintf("(%.1f%%)\n", 100*mean(out$sobre_ruta)))
cat("\nCross-check: geometric flag vs text-based flag (tipo, from the address)\n")
tab <- out[, .N, by=.(tipo_texto, sobre_ruta)][order(tipo_texto, sobre_ruta)]
print(dcast(tab, tipo_texto ~ sobre_ruta, value.var="N", fill=0))
cat("\n  - tipo 'ruta' with sobre_ruta=1: address and geometry agree (expected to be high)\n")
cat("  - tipo 'urbana' with sobre_ruta=1: highways crossing a town, which the address does not reveal\n")
cat("\nmedian d_ruta_km by text-based tipo:\n")
print(out[, .(mediana_km = round(median(d_ruta_km),3), n = .N), by = tipo_texto])

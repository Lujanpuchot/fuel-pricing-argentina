# 03_geocode_localities.R
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
# rebuilt here and stays on the exact subset. nearby_rivals_refinery.R builds it, and
# writes variables_espaciales_estacion.csv again from the final coordinates.
# Scripts 04 to 06 edit geocodificacion_final.csv in place: rerun them after this one.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(sf)
  library(fs)
})
source("code/00_config.R")
sf::sf_use_s2(TRUE)
options(timeout = 60)

DIR   <- DIR_INTERIM
GEO1  <- file.path(DIR, "geocodificacion_estaciones.csv")
GEO2  <- file.path(DIR, "geocodificacion_pass2.csv")
CENTL <- file.path(DIR, "centroides_localidad.csv")
FINAL <- file.path(DIR, "geocodificacion_final.csv")
OUT_E <- file.path(DIR, "variables_espaciales_estacion.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))
GJ   <- function(u) tryCatch(jsonlite::fromJSON(u), error = function(e) NULL)

# 1. Current state: first pass plus the second-pass rescues ----
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

# 2. Province ids (georef) ----
pv <- GJ("https://apis.datos.gob.ar/georef/api/provincias?max=30")$provincias
p_id <- as.character(pv$id); p_nom <- norm(pv$nombre); mapid <- setNames(p_id, p_nom)
mapid[["CAPITAL FEDERAL"]]  <- p_id[grepl("AUTONOMA", p_nom)][1]
mapid[["TIERRA DEL FUEGO"]] <- p_id[grepl("TIERRA DEL FUEGO", p_nom)][1]

# 3. Locality centroids (cached, resumable) ----
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

# 4. Graded precision and consolidated file ----
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

# 5. Distance to the nearest refinery (exact and locality levels) ----
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

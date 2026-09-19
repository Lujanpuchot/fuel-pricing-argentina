# 06_official_coordinates.R
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

suppressPackageStartupMessages(library(data.table))
source("code/00_config.R")

DIR_INT <- DIR_INTERIM
FIN <- file.path(DIR_INT, "geocodificacion_final.csv")
CO  <- file.path(DIR_INT, "coords_oficiales_energia.csv")
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

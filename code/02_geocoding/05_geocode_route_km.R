# 05_geocode_route_km.R
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

suppressPackageStartupMessages({library(data.table)})
source("code/00_config.R")

DIR_INT <- DIR_INTERIM
FIN    <- file.path(DIR_INT, "geocodificacion_final.csv")
POSTES <- file.path(DIR_INT, "postes_km_dnv.csv")
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

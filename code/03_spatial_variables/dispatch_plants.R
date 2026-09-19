# dispatch_plants.R
# Roster of dispatch plants by brand, built from the wholesale data, and distance
# from each station to the nearest plant of its own brand (the actual freight leg).
#
# Input:  precios_mayoristas_res1104.csv (wholesale prices and volumes, Res. 1104)
#         centroides_localidad.csv, geocodificacion_final.csv,
#         variables_refineria_marca.csv (from own_brand_refinery.R)
# Output: plantas_despacho_marca.csv     geocoded roster of plants, by year
#         variables_planta_estacion.csv  station x year, with d_planta_marca_km
#
# The wholesale data record each sale at the facility of the distributor
# (`localidadbunker`): YPF sells from about 112 localities and Shell from about
# 75, so dealers buy at dispatch plants rather than at the refinery.
# d_refineria_marca_km measures the distance to production; the variable built
# here measures the distance to dispatch, the trip made by the dealer's truck.
#
# Steps:
#  1) From the wholesale data, a plant is a (brand family x localidadbunker x
#     year) cell with volume sold through the station channels (resale,
#     consignment and company-owned stations).
#  2) Plants are geocoded at the centroid of their locality (own cache, then
#     georef); names of the form "Conjunto A - B - C" are mapped to the main city.
#  3) d_planta_marca_km is the geodesic distance from each station with exact
#     coordinates to the nearest plant of its family active in that year
#     (unbranded stations: any plant).

suppressPackageStartupMessages({library(data.table); library(sf); library(jsonlite)})
source("code/00_config.R")
sf_use_s2(TRUE)
INT  <- DIR_INTERIM
MAY  <- file.path(DIR_WHOLESALE, "precios_mayoristas_res1104.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

# 1. Active plants by family x locality x year (station channels only) ----
M <- fread(MAY, encoding="UTF-8",
           select=c("anio","operador","localidadbunker","provincia",
                    "canal_de_comercializacion","volumen"))
M <- M[grepl("^Estaciones|^Reventa", canal_de_comercializacion) & volumen > 0]
fam_op <- function(o){ s <- norm(o)
  fifelse(grepl("^YPF", s), "YPF",
  fifelse(grepl("SHELL|RAIZEN", s), "SHELL",
  fifelse(grepl("ESSO|AXION", s), "AXION",
  fifelse(grepl("PETROBRAS|EG3|PAMPA|TRAFIGURA|PUMA", s), "PUMA",
  fifelse(grepl("REFINERIA DEL NORTE|REFINOR", s), "REFINOR",
  fifelse(grepl("^OIL COMBUSTIBLES", s), "OIL", "OTRAS"))))))}
M[, familia := fam_op(operador)]
PL <- M[, .(vol_m3 = sum(volumen)), by=.(familia, localidadbunker, provincia, anio)]
cat("plant-years (family x locality x year):", nrow(PL),
    "| unique localities:", uniqueN(PL$localidadbunker), "\n")

# 2. Geocode the plant localities ----
# "Conjunto A - B ..." -> main city (manual mapping)
cab <- c("Conjunto Bahia Blanca - Rosales - Ing. White-Puerto Galván" = "BAHIA BLANCA",
  "Conjunto San Lorenzo - San Martín -  Rosario - Gral Lagos - Punta Alvear y Villa Constitución" = "SAN LORENZO",
  "Conjunto Zárate-Zarate Port - D. Dock - Campana" = "ZARATE",
  "Conjunto Comodoro Rivadavia-Muelle YPF" = "COMODORO RIVADAVIA")
locs <- unique(PL[, .(localidadbunker, provincia)])
locs[, loc_geo := fifelse(localidadbunker %in% names(cab), cab[localidadbunker],
                          norm(localidadbunker))]
# Common name fixes
locs[loc_geo == "S. M. DEL TUCUMAN", loc_geo := "SAN MIGUEL DE TUCUMAN"]
locs[grepl("^CONJUNTO", loc_geo), loc_geo := norm(sub("^Conjunto +([^-]+).*", "\\1", localidadbunker))]
locs[, pk := norm(provincia)]

cent <- fread(file.path(INT, "centroides_localidad.csv"), encoding="UTF-8")[!is.na(llat)]
cent[, `:=`(pk = norm(provincia), lk = norm(localidad))]
locs <- merge(locs, unique(cent[, .(pk, lk, llat, llon)]),
              by.x=c("pk","loc_geo"), by.y=c("pk","lk"), all.x=TRUE)
falt <- locs[is.na(llat)]
cat("geocoded from the cache:", locs[!is.na(llat), .N], "| missing:", nrow(falt), "\n")
# Fallback: georef /localidades by name and province
if (nrow(falt)) for (i in seq_len(nrow(falt))) {
  u <- sprintf("https://apis.datos.gob.ar/georef/api/localidades?nombre=%s&provincia=%s&max=1&campos=centroide",
               utils::URLencode(falt$loc_geo[i], reserved=TRUE),
               utils::URLencode(falt$pk[i], reserved=TRUE))
  r <- tryCatch(fromJSON(u), error=function(e) NULL); Sys.sleep(0.15)
  if (!is.null(r) && length(r$localidades) && nrow(r$localidades)) {
    locs[localidadbunker == falt$localidadbunker[i] & pk == falt$pk[i],
         `:=`(llat = r$localidades$centroide$lat[1], llon = r$localidades$centroide$lon[1])]
  }
}
# Entered by hand (found in neither the cache nor georef); coordinates at the
# locality or port level, off by a few km
man <- data.table(
  lb  = c("Capital Federal","Saladillo","Ing. Guillermo N. Juárez","COLONIA 25 DE MAYO",
          "S. M. de los Andes","Punta Loyola"),
  mlat = c(-34.6037,-35.6396,-23.8970,-37.7700,-40.1572,-51.6050),
  mlon = c(-58.3816,-59.7778,-61.8500,-67.7200,-71.3533,-69.0100))
locs <- merge(locs, man, by.x="localidadbunker", by.y="lb", all.x=TRUE)
locs[is.na(llat) & !is.na(mlat), `:=`(llat = mlat, llon = mlon)]
locs[, c("mlat","mlon") := NULL]
cat("geocoded in total:", locs[!is.na(llat), .N], "of", nrow(locs), "\n")
if (nrow(locs[is.na(llat)])) { cat("Not geocoded (check by hand):\n")
  print(locs[is.na(llat), .(localidadbunker, provincia)]) }

PL <- merge(PL, locs[, .(localidadbunker, provincia, loc_geo, llat, llon)],
            by=c("localidadbunker","provincia"), all.x=TRUE)
fwrite(PL[, .(familia, localidadbunker, loc_geo, provincia, anio, vol_m3,
              lat=llat, lon=llon)],
       file.path(INT, "plantas_despacho_marca.csv"), na="NA", bom=TRUE)
cat("[plantas_despacho_marca.csv]", nrow(PL), "plant-year rows\n")

# 3. d_planta_marca_km by station x year ----
G  <- fread(file.path(INT, "geocodificacion_final.csv"), encoding="UTF-8")
FR <- fread(file.path(INT, "variables_refineria_marca.csv"), encoding="UTF-8")
ex <- merge(G[precision=="exacta" & !is.na(lat), .(nro_inscripcion, slat=lat, slon=lon)],
            FR[, .(nro_inscripcion, anio, familia)], by="nro_inscripcion")
P  <- PL[!is.na(llat)]
pu <- unique(P[, .(familia, loc_geo, llat, llon)])          # unique plants by family
pe <- st_as_sf(unique(ex[, .(nro_inscripcion, slat, slon)]), coords=c("slon","slat"), crs=4326)
res <- list()
for (f in unique(ex$familia)) {
  pf <- if (f %in% pu$familia && f != "OTRAS") pu[familia == f] else unique(pu[, .(loc_geo, llat, llon)])
  pp <- st_as_sf(pf, coords=c("llon","llat"), crs=4326)
  DM <- matrix(as.numeric(st_distance(pe, pp))/1000, nrow=nrow(pe))   # stations x plants of f, in km
  colnames(DM) <- pf$loc_geo
  # Years in which each plant of the family is active (OTRAS: plants of any family)
  act <- if (f != "OTRAS") P[familia == f, .(anio = unique(anio)), by=loc_geo] else
         unique(P[, .(loc_geo, anio)])
  eaf <- ex[familia == f]
  eaf[, fila := match(nro_inscripcion, unique(ex$nro_inscripcion))]
  out <- eaf[, {
    pls <- act[anio == .BY$anio, loc_geo]; pls <- intersect(pls, colnames(DM))
    if (!length(pls)) .(nro_inscripcion = nro_inscripcion, d = NA_real_, pl = NA_character_)
    else { sub <- DM[fila, pls, drop=FALSE]
           .(nro_inscripcion = nro_inscripcion,
             d = apply(sub, 1, min), pl = pls[apply(sub, 1, which.min)]) }
  }, by=anio]
  out[, familia := f]; res[[f]] <- out
}
R <- rbindlist(res)[, .(nro_inscripcion, anio, familia,
                        planta_cercana = pl, d_planta_marca_km = round(d, 1))]
setorder(R, nro_inscripcion, anio)
fwrite(R, file.path(INT, "variables_planta_estacion.csv"), na="NA", bom=TRUE)

cat("\n[variables_planta_estacion.csv]", nrow(R), "station-year rows\n")
cat("\nmedian distance to the own-brand plant vs the own-brand refinery, by family:\n")
cmp <- merge(R, FR[, .(nro_inscripcion, anio, d_ref = d_refineria_marca_km)],
             by=c("nro_inscripcion","anio"))
print(cmp[!is.na(d_planta_marca_km),
          .(d_planta = round(median(d_planta_marca_km)), d_refineria = round(median(d_ref)),
            n = .N), by=familia][order(-n)])

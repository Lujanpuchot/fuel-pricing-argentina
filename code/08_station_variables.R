# 08_station_variables.R
# Builds the station-level variables that describe each outlet's competitive
# and cost environment. Each part writes its own file and none of them
# modifies the analysis panel; they are merged into it by key when the
# estimation sample is assembled.
#
# Input:  the analysis panel and geocodificacion_final.csv
# Output: one file per block of station variables, in DIR_INTERIM
#
# The parts are independent of each other.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
  library(sf)
  library(jsonlite)
  library(rnaturalearth)   # section 5, outline of the neighboring countries
})

source("code/00_config.R")

DIR_INT <- DIR_INTERIM
INT     <- DIR_INTERIM

# 1. Nearby rivals and distance to a refinery ----

# Spatial variables of the model: geodesic distance from each station to the
# nearest refinery, and distance to and number of nearby rival stations by year.
#
# Input:  geocodificacion_final.csv (consolidated output of code/07_geocode_stations.R)
#         eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: variables_espaciales_estacion.csv  one row per boca (outlet), time-invariant
#           d_refineria_km      km to the nearest refinery (cost shifter)
#         variables_espaciales_panel.csv     outlet x year
#           d_rival_min         km to the nearest rival station
#           d_rival_otra_marca  km to the nearest rival of another bandera (brand)
#           d_ypf_min           km to the nearest YPF station (for the maverick channel)
#           n_riv_1km/3km/5km   number of rivals within that radius
#           cobertura_exacta    % of outlets in the market-year with exact coordinates
#
# Both outputs are separate files keyed by nro_inscripcion; the analysis panel is
# only read.
#
# Rival variables use only outlets with precision == "exacta". The others sit at
# a centroid shared with the rest of their locality or department, which would
# give spurious zero distances. The set of rivals is therefore incomplete, and
# cobertura_exacta tells in which markets the distances can be trusted: where it
# is low they are poorly measured.
#
# The panel is yearly rather than monthly: the set of stations in a market
# changes slowly, and a monthly panel would cost 12 times as much to compute
# without adding information.

sf::sf_use_s2(TRUE)   # geodesic distances on lon/lat

FIN   <- file.path(DIR_INT, "geocodificacion_final.csv")
BASE  <- file.path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
OUT_E <- file.path(DIR_INT, "variables_espaciales_estacion.csv")
OUT_P <- file.path(DIR_INT, "variables_espaciales_panel.csv")

# Refineries. Coordinates are approximate (locality level, off by a few km). This
# is immaterial for distances of hundreds of km, but they should be checked
# before any use that needs fine precision.
REF <- data.table(
  refineria = c("La Plata (YPF)","Luján de Cuyo (YPF)","Plaza Huincul (YPF)",
                "Dock Sud (Raízen/Shell)","Campana (Axion/PAE)",
                "Bahía Blanca (Trafigura/Puma)","Campo Durán (Refinor)"),
  lat = c(-34.86, -33.03, -38.93, -34.65, -34.17, -38.75, -22.20),
  lon = c(-57.90, -68.88, -69.20, -58.34, -58.96, -62.27, -63.70))

# 1.1 Consolidated geocoding ----
g <- fread(FIN, encoding = "UTF-8")
cat("geocodificacion_final:", nrow(g), "| exact:", sum(g$precision == "exacta"),
    sprintf("(%.1f%%)", 100*mean(g$precision == "exacta")),
    "| locatable (exacta + localidad):", sum(g$precision %in% c("exacta","localidad")), "\n")
# (A) The refinery distance uses every locatable station (exacta or localidad):
#     refineries are hundreds of km away, so the locality centroid is enough.
# (B) Rival distances use exact coordinates only: the locality centroid puts all
#     the stations of a town at the same point (spurious zero distance).
ubic  <- g[precision %in% c("exacta","localidad") & !is.na(lat) & !is.na(lon)]
exact <- g[precision == "exacta" & !is.na(lat) & !is.na(lon)]

# 1.2 (A) Distance to the nearest refinery, by station ----
pe <- st_as_sf(ubic, coords = c("lon","lat"), crs = 4326)
pr <- st_as_sf(REF,  coords = c("lon","lat"), crs = 4326)
idx <- st_nearest_feature(pe, pr)
dref <- as.numeric(st_distance(pe, pr[idx, ], by_element = TRUE)) / 1000
estac <- data.table(nro_inscripcion = ubic$nro_inscripcion,
                    precision = ubic$precision,
                    refineria_cercana = REF$refineria[idx],
                    d_refineria_km = round(dref, 1))
fwrite(estac, OUT_E, na = "NA", bom = TRUE)
cat("(A) distance to refinery saved to", basename(OUT_E), "|",
    "median", round(median(estac$d_refineria_km)), "km\n")

# 1.3 (B) Outlet x year panel: nearby rivals ----
b <- readRDS(BASE); setDT(b)
act <- unique(b[canal_de_comercializacion == "Al público",
                .(nro_inscripcion, anio = as.integer(anio), bandera, departamento, provincia)])
rm(b); invisible(gc())
act <- act[!is.na(anio)]
act[, mercado := paste0(provincia, "||", departamento)]

# Coverage by market-year (quality of the measurement)
act[, exacta := nro_inscripcion %in% exact$nro_inscripcion]
cob <- act[, .(cobertura_exacta = round(100*mean(exacta), 1), n_bocas = .N),
           by = .(mercado, anio)]

ae <- merge(act[exacta == TRUE], exact[, .(nro_inscripcion, lat, lon)], by = "nro_inscripcion")
anios <- sort(unique(ae$anio))
# Test runs: set the environment variable ESP_N_ANIOS to keep only the last n years
.n_test <- suppressWarnings(as.integer(Sys.getenv("ESP_N_ANIOS", "0")))
if (!is.na(.n_test) && .n_test > 0) { anios <- tail(anios, .n_test); cat("Test run, years:", paste(anios, collapse=","), "\n") }

R_CAND <- 50000  # candidate rivals within 50 km; beyond that there is no local competition
res <- rbindlist(lapply(anios, function(y){
  d <- ae[anio == y]
  if (nrow(d) < 2) return(NULL)
  p <- st_as_sf(d, coords = c("lon","lat"), crs = 4326)
  cand <- st_is_within_distance(p, p, dist = R_CAND)          # uses the spatial index
  out <- rbindlist(lapply(seq_len(nrow(d)), function(i){
    j <- setdiff(cand[[i]], i)                                # drop the outlet itself
    if (!length(j)) return(data.table(nro_inscripcion = d$nro_inscripcion[i], anio = y,
                                      d_rival_min = NA_real_, d_rival_otra_marca = NA_real_,
                                      d_ypf_min = NA_real_, n_riv_1km = 0L, n_riv_3km = 0L, n_riv_5km = 0L))
    dd <- as.numeric(st_distance(p[i, ], p[j, ]))/1000
    om <- d$bandera[j] != d$bandera[i]
    yp <- d$bandera[j] == "YPF"
    data.table(nro_inscripcion = d$nro_inscripcion[i], anio = y,
               d_rival_min        = round(min(dd), 3),
               d_rival_otra_marca = if (any(om)) round(min(dd[om]), 3) else NA_real_,
               d_ypf_min          = if (any(yp)) round(min(dd[yp]), 3) else NA_real_,
               n_riv_1km = sum(dd <= 1), n_riv_3km = sum(dd <= 3), n_riv_5km = sum(dd <= 5))
  }))
  cat("  ", y, ": ", nrow(d), " outlets with exact coordinates\n", sep = "")
  out
}))

res <- merge(res, act[, .(nro_inscripcion, anio, mercado)], by = c("nro_inscripcion","anio"), all.x = TRUE)
res <- merge(res, cob, by = c("mercado","anio"), all.x = TRUE)
fwrite(res, OUT_P, na = "NA", bom = TRUE)
cat("\n(B) outlet x year panel saved to", basename(OUT_P), "| rows:", nrow(res), "\n")
# median() of integers returns an integer when N is odd and a double when N is
# even; data.table stops on the inconsistent types across groups, hence as.numeric().
print(res[, .(mediana_d_rival = round(median(d_rival_min, na.rm = TRUE), 2),
              mediana_n_5km   = as.numeric(median(n_riv_5km)),
              cobertura_media = round(mean(cobertura_exacta, na.rm = TRUE), 1)), by = anio][order(anio)])

# 2. Refinery of the station's own brand ----

# Distance from each station to the nearest refinery of its own brand, by
# station-year (the bandera (brand) of a station changes with rebrandings).
#
# Input:  geocodificacion_final.csv
#         eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: variables_refineria_marca.csv (station x year)
#
# d_refineria_km, from section 1, is the distance to the nearest
# refinery of any brand. A Shell station does not buy from YPF, though: the
# relevant logistic cost is the distance to a refinery of its own chain, which is
# what d_refineria_marca_km measures.
#
# Chain -> refineries. Brands are grouped in families, so that the mapping is
# stable across rebrandings:
#   YPF                             -> La Plata, Luján de Cuyo, Plaza Huincul
#   Shell / Raízen                  -> Dock Sud
#   Esso -> Axion (PAE)             -> Campana
#   EG3 / Petrobras / Pampa -> Puma -> Bahía Blanca
#   Refinor                         -> Campo Durán
#   Oil Combustibles                -> San Lorenzo (an eighth refinery, added here)
#   Unbranded and other brands      -> the nearest refinery of any brand (they buy
#                                      in the wholesale market, not tied to a chain)
#
# This is an approximation: actual logistics run through dispatch terminals
# (pipelines and coastal shipping), not only refineries. The variable captures
# the origin of the chain; distance to the dispatch plants is built in
# section 3.

sf_use_s2(TRUE)
BASE <- file.path(INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

REF <- data.table(
  refineria = c("La Plata (YPF)","Luján de Cuyo (YPF)","Plaza Huincul (YPF)",
                "Dock Sud (Raízen/Shell)","Campana (Axion/PAE)",
                "Bahía Blanca (Trafigura/Puma)","Campo Durán (Refinor)",
                "San Lorenzo (Oil)"),
  lat = c(-34.86, -33.03, -38.93, -34.65, -34.17, -38.75, -22.20, -32.72),
  lon = c(-57.90, -68.88, -69.20, -58.34, -58.96, -62.27, -63.70, -60.75))
FAM <- list(  # family -> rows of REF
  YPF     = 1:3, SHELL = 4L, AXION = 5L, PUMA = 6L, REFINOR = 7L, OIL = 8L)
familia_de <- function(b){   # bandera, as spelled in the data over the years -> brand family
  s <- norm(b)
  fifelse(grepl("YPF", s), "YPF",
  fifelse(grepl("SHELL|RAIZEN", s), "SHELL",
  fifelse(grepl("ESSO|AXION", s), "AXION",
  fifelse(grepl("PETROBRAS|PUMA|EG3|PAMPA", s), "PUMA",
  fifelse(grepl("REFINOR", s), "REFINOR",
  fifelse(grepl("^OIL\\b|OIL COMBUSTIBLES", s), "OIL", "OTRAS"))))))
}

# Locatable stations and matrix of distances to the 8 refineries ----
g <- fread(file.path(INT,"geocodificacion_final.csv"), encoding="UTF-8")
ub <- g[precision %in% c("exacta","localidad") & !is.na(lat)]
pe <- st_as_sf(ub, coords=c("lon","lat"), crs=4326)
pr <- st_as_sf(REF, coords=c("lon","lat"), crs=4326)
DM <- matrix(as.numeric(st_distance(pe, pr))/1000, nrow=nrow(ub))  # stations x 8, in km

# Minimum distance by family, for every station
dfam <- data.table(nro_inscripcion = ub$nro_inscripcion)
for (f in names(FAM)) {
  sub <- DM[, FAM[[f]], drop=FALSE]
  dfam[[paste0("d_",f)]]   <- apply(sub, 1, min)
  dfam[[paste0("ref_",f)]] <- REF$refineria[FAM[[f]]][apply(sub, 1, which.min)]
}
dfam[, d_OTRAS := apply(DM, 1, min)]
dfam[, ref_OTRAS := REF$refineria[apply(DM, 1, which.min)]]

# Bandera by station-year, from the analysis panel (read only) ----
b <- readRDS(BASE); setDT(b)
act <- unique(b[canal_de_comercializacion == "Al público" & !is.na(bandera),
                .(nro_inscripcion, anio = as.integer(anio), bandera)])
rm(b); invisible(gc())
act <- act[!is.na(anio)]
act[, familia := familia_de(bandera)]
cat("station-years with bandera:", nrow(act), "| families:\n")
print(act[, .N, by=familia][order(-N)])

# Keep, for each station-year, the distance of its own family ----
out <- merge(act, dfam, by="nro_inscripcion")          # locatable stations only
out[, d_refineria_marca_km := round(fifelse(familia=="YPF", d_YPF,
        fifelse(familia=="SHELL", d_SHELL, fifelse(familia=="AXION", d_AXION,
        fifelse(familia=="PUMA", d_PUMA, fifelse(familia=="REFINOR", d_REFINOR,
        fifelse(familia=="OIL", d_OIL, d_OTRAS)))))), 1)]
out[, refineria_marca := fifelse(familia=="YPF", ref_YPF,
        fifelse(familia=="SHELL", ref_SHELL, fifelse(familia=="AXION", ref_AXION,
        fifelse(familia=="PUMA", ref_PUMA, fifelse(familia=="REFINOR", ref_REFINOR,
        fifelse(familia=="OIL", ref_OIL, ref_OTRAS))))))]
res <- out[, .(nro_inscripcion, anio, bandera, familia, refineria_marca, d_refineria_marca_km)]
setorder(res, nro_inscripcion, anio)
fwrite(res, file.path(INT, "variables_refineria_marca.csv"), na="NA", bom=TRUE)

# Summary and checks ----
cat("\n[variables_refineria_marca.csv]", nrow(res), "station-year rows |",
    uniqueN(res$nro_inscripcion), "stations\n")
cat("\nmedian distance to the own-brand refinery vs the nearest refinery of any brand, by family:\n")
cmp <- merge(res, dfam[, .(nro_inscripcion, d_cualquiera = round(d_OTRAS,1))], by="nro_inscripcion")
print(cmp[, .(d_marca = round(median(d_refineria_marca_km)),
              d_cualquiera = round(median(d_cualquiera)), n_est_anio = .N), by=familia][order(-n_est_anio)])
cat("\nexample: SHELL stations in Mendoza (the province of the Luján de Cuyo refinery):\n")
mz <- merge(res[familia=="SHELL" & anio==2015], g[, .(nro_inscripcion, provincia)], by="nro_inscripcion")
print(mz[norm(provincia)=="MENDOZA", .(mediana_d_marca_km = round(median(d_refineria_marca_km)), n=.N)])

# 3. Dispatch plants ----

# Roster of dispatch plants by brand, built from the wholesale data, and distance
# from each station to the nearest plant of its own brand (the actual freight leg).
#
# Input:  precios_mayoristas_res1104.csv (wholesale prices and volumes, Res. 1104)
#         centroides_localidad.csv, geocodificacion_final.csv,
#         variables_refineria_marca.csv (from section 2)
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

sf_use_s2(TRUE)
MAY  <- file.path(DIR_WHOLESALE, "precios_mayoristas_res1104.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

# 3.1 Active plants by family x locality x year (station channels only) ----
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

# 3.2 Geocode the plant localities ----
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

# 3.3 d_planta_marca_km by station x year ----
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

# 4. On-highway indicator ----

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

sf_use_s2(TRUE)
CACHE<- file.path(INT, "raw_osm")   # OSM geometry, kept with the rest of the data

# 4.1 Trunk road network from the Overpass JSON ----
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

# 4.2 Stations with exact coordinates ----
g <- fread(file.path(INT, "geocodificacion_final.csv"), encoding = "UTF-8")
ex <- g[precision == "exacta" & !is.na(lat)]
pe <- st_as_sf(ex, coords = c("lon","lat"), crs = 4326)

# 4.3 Distance to the nearest road centerline ----
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

# 4.4 Summary and cross-check against the text-based flag ----
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

# 5. Borders, cities and regions ----

# Time-invariant geographic variables by station: distance to the border and to
# the nearest road crossing, to large cities and to Buenos Aires, Patagonia flag.
#
# Input:  geocodificacion_final.csv (locatable stations: precision exacta or localidad)
# Output: variables_geograficas_estacion.csv
#           d_frontera_km    km to the nearest international border (edge of the
#                            five neighboring countries)
#           d_cruce_km       km to the nearest international road crossing
#           cruce_cercano,
#           pais_cruce       name and country of that crossing
#           d_ciudad_km      km to the nearest large city (roughly above 100,000
#                            inhabitants)
#           ciudad_cercana   name of that city
#           d_gba_km         km to the city of Buenos Aires, the dominant market
#           patagonia_icl    1 if the province is in Patagonia (differential of the
#                            ICL, the federal tax on liquid fuels)
#
# The border variables are meant for cross-border arbitrage and the border zone
# of the ICL. d_ciudad_km and d_gba_km capture market size and demand density, as
# covariates of the demand model. patagonia_icl marks the geography of the
# Patagonian tax differential; it does not replicate the department-level tax
# rules. The output is a separate file keyed by nro_inscripcion.

sf::sf_use_s2(TRUE)   # geodesic distances

FIN   <- file.path(DIR_INT, "geocodificacion_final.csv")
OUT_G <- file.path(DIR_INT, "variables_geograficas_estacion.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

# 5.1 Locatable stations (exacta + localidad) ----
g <- fread(FIN, encoding = "UTF-8")
ubic <- g[precision %in% c("exacta","localidad") & !is.na(lat) & !is.na(lon)]
cat("locatable stations:", nrow(ubic), "\n")
pe <- st_as_sf(ubic, coords = c("lon","lat"), crs = 4326)

# 5.2 International border as a line ----
# Edge of the five neighboring countries; a secondary continuous variable. In the
# east and center the nearest border is the Río de la Plata or the Río Uruguay,
# which cannot be crossed by car, so the nearest country is not reported here.
# For arbitrage use the crossings in 5.3.
vecinos <- c("Chile","Bolivia","Paraguay","Brazil","Uruguay")
nb <- ne_countries(country = vecinos, scale = "large", returnclass = "sf")
nbu <- st_union(nb)
ubic[, d_frontera_km := round(as.numeric(st_distance(pe, nbu))/1000, 1)]

# 5.3 Nearest border crossing open to road traffic ----
# The relevant variable for arbitrage: 22 international road crossings, including
# the bridges to Uruguay.
cruces <- data.table(
  cruce = c("Cristo Redentor","Cardenal Samoré","Pino Hachado","Integración Austral",
            "Jama","San Francisco","Agua Negra","Pehuenche","Huemules","Río Turbio",   # Chile (10)
            "La Quiaca","Salvador Mazza","Aguas Blancas",                              # Bolivia (3)
            "Clorinda","Posadas-Encarnación",                                          # Paraguay (2)
            "Puerto Iguazú","Paso de los Libres","Santo Tomé","Bernardo de Irigoyen",  # Brazil (4)
            "Gualeguaychú-Fray Bentos","Colón-Paysandú","Concordia-Salto"),            # Uruguay (3)
  pais = c(rep("Chile",10), rep("Bolivia",3), rep("Paraguay",2), rep("Brasil",4), rep("Uruguay",3)),
  lat = c(-32.82,-40.72,-38.66,-52.13,-23.23,-26.92,-30.17,-35.98,-45.65,-51.57,
          -22.10,-22.05,-22.72, -25.28,-27.38, -25.60,-29.75,-28.55,-26.25,
          -33.10,-32.22,-31.28),
  lon = c(-70.05,-71.87,-70.90,-69.52,-67.00,-68.29,-69.83,-70.40,-71.65,-72.33,
          -65.60,-63.69,-64.35, -57.72,-55.87, -54.58,-57.09,-56.03,-53.64,
          -58.32,-58.14,-57.90))
pcx <- st_as_sf(cruces, coords = c("lon","lat"), crs = 4326)
ix  <- st_nearest_feature(pe, pcx)
ubic[, d_cruce_km  := round(as.numeric(st_distance(pe, pcx[ix, ], by_element = TRUE))/1000, 1)]
ubic[, cruce_cercano := cruces$cruce[ix]]
ubic[, pais_cruce    := cruces$pais[ix]]
cat("d_frontera_km (border line): median", round(median(ubic$d_frontera_km)), "km\n")
cat("d_cruce_km (road crossing): median", round(median(ubic$d_cruce_km)), "km |",
    "within 25 km of a crossing:", ubic[d_cruce_km < 25, .N], "stations\n")
print(ubic[, .N, by = pais_cruce][order(-N)])

# 5.4 Large cities ----
# Cities above roughly 100,000 inhabitants; coordinates of the urban center.
ciu <- data.table(
  ciudad = c("Buenos Aires","Córdoba","Rosario","La Plata","Mar del Plata","Tucumán",
             "Salta","Santa Fe","San Juan","Resistencia","Neuquén","Santiago del Estero",
             "Corrientes","Posadas","Bahía Blanca","Paraná","Mendoza","Formosa","Jujuy",
             "La Rioja","Río Cuarto","Comodoro Rivadavia","San Luis","Catamarca",
             "Concordia","San Nicolás","Río Gallegos","Ushuaia","Viedma","Trelew"),
  lat = c(-34.61,-31.42,-32.95,-34.92,-38.00,-26.82,-24.79,-31.63,-31.54,-27.45,-38.95,
          -27.80,-27.47,-27.37,-38.72,-31.73,-32.89,-26.18,-24.19,-29.41,-33.12,-45.86,
          -33.30,-28.47,-31.39,-33.34,-51.62,-54.80,-40.81,-43.25),
  lon = c(-58.38,-64.19,-60.66,-57.95,-57.56,-65.22,-65.41,-60.70,-68.54,-58.99,-68.06,
          -64.26,-58.83,-55.90,-62.27,-60.52,-68.84,-58.17,-65.30,-66.85,-64.35,-67.50,
          -66.34,-65.78,-58.02,-60.21,-69.22,-68.30,-63.00,-65.31))
pc <- st_as_sf(ciu, coords = c("lon","lat"), crs = 4326)
idx <- st_nearest_feature(pe, pc)
ubic[, d_ciudad_km := round(as.numeric(st_distance(pe, pc[idx, ], by_element = TRUE))/1000, 1)]
ubic[, ciudad_cercana := ciu$ciudad[idx]]
gba <- st_as_sf(data.frame(lat=-34.6037, lon=-58.3816), coords=c("lon","lat"), crs=4326)
ubic[, d_gba_km := round(as.numeric(st_distance(pe, gba))/1000, 0)]
cat("\nd_ciudad_km: median", round(median(ubic$d_ciudad_km)), "km |",
    "d_gba_km: median", round(median(ubic$d_gba_km)), "km\n")

# 5.5 Patagonia flag (Patagonian differential of the ICL) ----
PATAGONIA <- c("NEUQUEN","RIO NEGRO","CHUBUT","SANTA CRUZ","TIERRA DEL FUEGO")
ubic[, patagonia_icl := as.integer(norm(provincia) %in% PATAGONIA)]
cat("stations in Patagonia (ICL):", ubic[patagonia_icl==1, .N], "\n")

# 5.6 Save ----
out <- ubic[, .(nro_inscripcion, precision, provincia, localidad,
                d_frontera_km, d_cruce_km, cruce_cercano, pais_cruce,
                d_ciudad_km, ciudad_cercana, d_gba_km, patagonia_icl)]
fwrite(out, OUT_G, na = "NA", bom = TRUE)
cat("\n[saved", basename(OUT_G), "-", nrow(out), "stations]\n")
cat("\nsummary:\n")
print(out[, .(d_frontera_km = round(median(d_frontera_km)),
              d_ciudad_km   = round(median(d_ciudad_km)),
              d_gba_km      = round(median(d_gba_km))), by = patagonia_icl])

# 6. Station amenities ----

# Station amenities (shop, CNG, 24 h, official vertical form), from a spatial
# match between the panel stations and the brands' station-locator listings.
#
# Input:  localizadores_marcas.csv (the brands' station-locator listings)
#         geocodificacion_final.csv
#         variables_refineria_marca.csv (from section 2)
# Output: variables_shop_estacion.csv
#
# The match is spatial and within brand family: a YPF station of the panel is
# compared only with the YPF points of the locator, and so on. Each station is
# matched to the nearest point if it lies within 250 m. The family of a panel
# station is that of its bandera (brand) in the last year observed. Only stations
# with exact coordinates are used.
#
# The listings are a 2026 snapshot, not a historical panel. They are meant for
# the official vertical form, second-stage regressions, validation and
# descriptive statistics, not for mean utility, where the fixed effects absorb
# them.

sf_use_s2(TRUE)
MAXD <- 250   # meters

L <- fread(file.path(INT, "localizadores_marcas.csv"), encoding="UTF-8")
G <- fread(file.path(INT, "geocodificacion_final.csv"), encoding="UTF-8")
FR <- fread(file.path(INT, "variables_refineria_marca.csv"), encoding="UTF-8")

# Brand family of each panel station (last year observed)
fam <- FR[order(nro_inscripcion, anio)][, .SD[.N], by=nro_inscripcion][, .(nro_inscripcion, familia)]
ex <- merge(G[precision=="exacta" & !is.na(lat)], fam, by="nro_inscripcion")
cat("stations with exact coordinates and a family:", nrow(ex), "\n")

# mias: panel stations of the family; suyas: locator points of the same brand
res <- list()
for (f in c("YPF","SHELL","AXION","PUMA")) {
  mias <- ex[familia == f]
  suyas <- L[marca == f]
  if (!nrow(mias) || !nrow(suyas)) next
  pm <- st_as_sf(mias,  coords=c("lon","lat"), crs=4326)
  ps <- st_as_sf(suyas, coords=c("lon","lat"), crs=4326)
  idx <- st_nearest_feature(pm, ps)
  d   <- as.numeric(st_distance(pm, ps[idx,], by_element=TRUE))
  ok  <- d <= MAXD
  cat(sprintf("  %-6s panel %4d | locator %4d | match<=%dm: %4d (%.0f%%)\n",
      f, nrow(mias), nrow(suyas), MAXD, sum(ok), 100*mean(ok)))
  res[[f]] <- data.table(mias[, .(nro_inscripcion, familia)],
                         dist_m = round(d,1), match = ok,
                         suyas[idx, .(id_marca, tienda, tienda_tipo, gnc_loc=gnc,
                                      lubricentro, h24, banios, ev, forma_vertical,
                                      inicio_actividades, cuit_loc=cuit)])
}
R <- rbindlist(res, use.names=TRUE)
R[match == FALSE, c("id_marca","tienda","tienda_tipo","gnc_loc","lubricentro","h24",
                    "banios","ev","forma_vertical","inicio_actividades","cuit_loc") := NA]
fwrite(R, file.path(INT, "variables_shop_estacion.csv"), na="NA", bom=TRUE)

cat("\n[variables_shop_estacion.csv]", nrow(R), "rows |", R[match==TRUE,.N], "matched\n")
cat("\ncoverage of the shop attribute (tienda) among matched stations:\n")
print(R[match==TRUE, .(n=.N, con_tienda=sum(tienda,na.rm=TRUE),
                       pct=round(100*mean(tienda,na.rm=TRUE),1)), by=familia])
cat("\nofficial vertical form (YPF RED_PROPIA / AXION MOSO):\n")
print(R[match==TRUE & !is.na(forma_vertical), .N, by=.(familia, forma_vertical)][order(familia,-N)])
cat("\nCNG check: locator vs dual type from the address (YPF+AXION+PUMA sample)\n")


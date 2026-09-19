# nearby_rivals_refinery.R
# Spatial variables of the model: geodesic distance from each station to the
# nearest refinery, and distance to and number of nearby rival stations by year.
#
# Input:  geocodificacion_final.csv (consolidated output of code/02_geocoding)
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

suppressPackageStartupMessages({library(data.table); library(sf)})
source("code/00_config.R")
sf::sf_use_s2(TRUE)   # geodesic distances on lon/lat

DIR_INT <- DIR_INTERIM
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

# 1. Consolidated geocoding ----
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

# 2. (A) Distance to the nearest refinery, by station ----
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

# 3. (B) Outlet x year panel: nearby rivals ----
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

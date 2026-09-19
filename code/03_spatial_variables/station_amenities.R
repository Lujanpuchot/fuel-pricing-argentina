# station_amenities.R
# Station amenities (shop, CNG, 24 h, official vertical form), from a spatial
# match between the panel stations and the brands' station-locator listings.
#
# Input:  localizadores_marcas.csv (the brands' station-locator listings)
#         geocodificacion_final.csv
#         variables_refineria_marca.csv (from own_brand_refinery.R)
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

suppressPackageStartupMessages({library(data.table); library(sf)})
source("code/00_config.R")
sf_use_s2(TRUE)
INT  <- DIR_INTERIM
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

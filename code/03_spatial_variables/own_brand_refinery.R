# own_brand_refinery.R
# Distance from each station to the nearest refinery of its own brand, by
# station-year (the bandera (brand) of a station changes with rebrandings).
#
# Input:  geocodificacion_final.csv
#         eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: variables_refineria_marca.csv (station x year)
#
# d_refineria_km, from nearby_rivals_refinery.R, is the distance to the nearest
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
# dispatch_plants.R.

suppressPackageStartupMessages({library(data.table); library(sf)})
source("code/00_config.R")
sf_use_s2(TRUE)
INT  <- DIR_INTERIM
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

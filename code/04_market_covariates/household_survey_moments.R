# household_survey_moments.R
# Fuel expenditure by income decile and car ownership by province, from the
# 2017-18 household expenditure survey (ENGHo, INDEC).
#
# Input:  engho2018_gastos.txt, engho2018_hogares.txt (INDEC public microdata,
#         downloaded to raw_engho/ if missing)
# Output: covar_engho_micromomentos.csv, covar_engho_motorizacion.csv
#
# ENGHo records the detailed expenditure of each household (COICOP), its income,
# region and car ownership. Two things are built from it:
#  (1) Micro-moments for the BLP model (as in Petrin, or the micro moments of
#      Conlon and Gortmaker): share of households that buy fuel and budget share
#      of fuel, by decile of per capita income and by region. They discipline the
#      income-price interaction in the GMM without the need for local income data.
#  (2) Car ownership by province, an input for a "physical" market size
#      (population x ownership rate x average consumption), used as a robustness
#      check.
#
# Fuel expenditure is COICOP class "A0722" (fuels and lubricants for personal
# transport equipment). A household owns a car when propauto >= 2: the variable
# is coded 1 = none, 2 = one car, 3 = two or more (checked against the equipment
# module: it reproduces the 40.6% of CABA). Weights are in pondera.
#
# Only the 2017-18 wave is processed; the 2004-05 and 2012-13 waves, needed for a
# motorization series, are still pending (RAR files).

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})
source("code/00_config.R")
options(timeout = 300)

DIR  <- DIR_COVAR
RAW  <- fs::path(DIR, "raw_engho")
dir.create(RAW, showWarnings=FALSE, recursive=TRUE)
BASE_URL <- "https://www.indec.gob.ar/ftp/cuadros/menusuperior/engho"
for (z in c("engho2018_gastos.zip","engho2018_hogares.zip")) {
  fz <- fs::path(RAW, z)
  if (!fs::file_exists(fz)) {
    download.file(paste0(BASE_URL,"/",z), fz, mode="wb", quiet=TRUE)
    unzip(fz, exdir=RAW)
  }
  ft <- fs::path(RAW, sub("zip$","txt",z))
  if (!fs::file_exists(ft)) unzip(fz, exdir=RAW)
}

G <- fread(fs::path(RAW,"engho2018_gastos.txt"), sep="|", encoding="UTF-8",
           select=c("id","clase","monto"))
H <- fread(fs::path(RAW,"engho2018_hogares.txt"), sep="|", encoding="UTF-8",
           select=c("id","provincia","region","pondera","gastot","ingtoth","ingpch","propauto"))

# Fuel expenditure per household (zero if none is reported)
comb <- G[clase=="A0722", .(gasto_comb = sum(monto)), by=id]
D <- merge(H, comb, by="id", all.x=TRUE)
D[is.na(gasto_comb), gasto_comb := 0]
D <- D[gastot > 0 & ingtoth > 0]

# Weighted deciles of per capita household income
setorder(D, ingpch)
D[, decil := pmin(10L, 1L + as.integer(10*cumsum(pondera)/sum(pondera) - 1e-9))]

# Moments by income decile (MM) and by INDEC region (MR)
MM <- D[, .(nivel = "decil_ingreso_pc", grupo = as.character(decil[1]),
            hogares_muestra = .N,
            pct_compra_combustible = round(100*weighted.mean(gasto_comb>0, pondera), 2),
            share_presupuesto      = round(100*weighted.mean(gasto_comb/gastot, pondera), 3),
            share_si_compra        = round(100*weighted.mean((gasto_comb/gastot)[gasto_comb>0],
                                                             pondera[gasto_comb>0]), 3),
            pct_tiene_auto         = round(100*weighted.mean(propauto>=2, pondera), 2)), by=decil]
MR <- D[, .(nivel = "region_indec", grupo = as.character(region[1]),
            hogares_muestra = .N,
            pct_compra_combustible = round(100*weighted.mean(gasto_comb>0, pondera), 2),
            share_presupuesto      = round(100*weighted.mean(gasto_comb/gastot, pondera), 3),
            share_si_compra        = round(100*weighted.mean((gasto_comb/gastot)[gasto_comb>0],
                                                             pondera[gasto_comb>0]), 3),
            pct_tiene_auto         = round(100*weighted.mean(propauto>=2, pondera), 2)), by=region]
OUT <- rbind(MM[, !"decil"], MR[, !"region"])
fwrite(OUT, fs::path(DIR, "covar_engho_micromomentos.csv"), bom=TRUE)

# Share of households with a car, by province
MOT <- D[, .(pct_hogares_con_auto = round(100*weighted.mean(propauto>=2, pondera), 2),
             hogares_muestra = .N, onda = "2017-18"), by=provincia][order(provincia)]
fwrite(MOT, fs::path(DIR, "covar_engho_motorizacion.csv"), bom=TRUE)

cat("[covar_engho_micromomentos.csv]", nrow(OUT), "rows (10 deciles + 6 regions)\n")
cat("[covar_engho_motorizacion.csv]", nrow(MOT), "provinces | 2017-18 wave\n")
cat("\nchecks: budget share d1 =", MM[decil==1, share_presupuesto],
    "% and d10 =", MM[decil==10, share_presupuesto],
    "% | car ownership d1 =", MM[decil==1, pct_tiene_auto], "% and d10 =", MM[decil==10, pct_tiene_auto], "%\n")

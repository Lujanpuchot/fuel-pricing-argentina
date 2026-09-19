# national_series.R
# National and international monthly series, 2004-01 to 2024-12: spliced CPI,
# Brent, US spot prices of refined products and the official exchange rate.
#
# Input:  public downloads from the datos.gob.ar series API and FRED (no account
#         or API key needed), cached in raw_series/
# Output: covar_series_nacionales.csv
#
# (1) Spliced CPI, December 2016 = 100. The official CPI is unreliable over
#     2007-2015 (intervention of INDEC), so monthly changes are chained across
#     three indices, as is standard in the literature:
#       2004-01 to 2005-11  INDEC CPI for Greater Buenos Aires (base April 2008)
#       2005-12 to 2016-11  San Luis CPI (DPEyC, base 2003), a reliable provincial index
#       2016-12 to 2024-12  INDEC national CPI (December 2016 = 100)
#     A citable alternative for robustness: CIFRA-CTA "IPC Provincias" (2007-2018).
# (2) Brent spot, USD per barrel (FRED MCOILBRENTEU, from EIA).
# (3) Spot prices of refined products in USD per gallon and per m3 (FRED, from
#     EIA): regular conventional gasoline and No. 2 ULSD diesel, US Gulf Coast
#     (USGC) and New York Harbor (NYH, as a backup). They feed the predicted
#     import parity for 2004-2009, when the parity observed in SESCO does not
#     exist. ULSD starts in June 2006; earlier months are spliced with NYH No. 2
#     heating oil (columns *_emp).
# (4) Official wholesale exchange rate (BCRA Communication A 3500), monthly
#     average from the datos.gob.ar API. It averages over calendar days, with
#     weekends repeating the Friday value; a business-day average would have to
#     be built from the BCRA file com3500.xls.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})
source("code/00_config.R")
options(timeout = 120)

DIR  <- DIR_COVAR
RAW  <- fs::path(DIR, "raw_series")
dir.create(RAW, showWarnings=FALSE, recursive=TRUE)
mfloor <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))

dl <- function(url, dest) if (!fs::file_exists(dest)) download.file(url, dest, mode="wb", quiet=TRUE)

# 1. CPI: the four indices in a single API call ----
U_IPC <- paste0("https://apis.datos.gob.ar/series/api/series/?ids=",
  "96.3_ING_2008_M_19,",            # CPI GBA, base April 2008 (1993-2013; reliable only up to 2006)
  "197.1_NIVEL_GENERAL_2014_0_13,", # CPI San Luis, base 2003 (from 2005-10)
  "193.1_NIVEL_GENERAL_JULI_0_13,", # CPI CABA (from 2012-07; for reference only)
  "148.3_INIVELNAL_DICI_M_26",      # national CPI (December 2016 = 100)
  "&format=csv&limit=1000&start_date=2004-01-01&header=ids")
dl(U_IPC, fs::path(RAW, "ipc_tramos.csv"))
ipc <- fread(fs::path(RAW, "ipc_tramos.csv"), encoding="UTF-8")
setnames(ipc, c("mes","gba","sanluis","caba","nacional"))
ipc[, mes := as.Date(mes)]

SLd16 <- ipc[mes==as.Date("2016-12-01"), sanluis]     # 1350.48
GBAd06 <- ipc[mes==as.Date("2006-12-01"), gba]        # 89.1559
stopifnot(is.finite(SLd16), is.finite(GBAd06))
ipc[, ipc_empalmado := fifelse(
      mes >= as.Date("2016-12-01"), nacional,
      fifelse(mes >= as.Date("2005-12-01"), 100 * sanluis / SLd16,     # San Luis segment (2005-12 to 2016-11)
              NA_real_))]
ipc_d05 <- ipc[mes==as.Date("2005-12-01"), ipc_empalmado]              # level at the end of the GBA segment
GBA_d05 <- ipc[mes==as.Date("2005-12-01"), gba]
ipc[mes < as.Date("2005-12-01"), ipc_empalmado := ipc_d05 * gba / GBA_d05]
ipc[, fuente_ipc := fifelse(mes >= as.Date("2016-12-01"), "indec_nacional",
                    fifelse(mes >= as.Date("2005-12-01"), "san_luis_encadenado",
                            "gba_indec_encadenado"))]
# The San Luis segment starts in December 2005 (its series begins in October
# 2005), so the GBA/San Luis splice falls well before the intervention.

# 2. Brent, monthly (FRED, USD/bbl) ----
U_BR <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MCOILBRENTEU&cosd=2004-01-01&coed=2024-12-31"
dl(U_BR, fs::path(RAW, "brent_fred.csv"))
br <- fread(fs::path(RAW, "brent_fred.csv"), encoding="UTF-8")
setnames(br, c("mes","brent_usd_bbl"))
br[, mes := mfloor(mes)]

# 3. Spot prices of refined products, USD/gal -> USD/m3 (FRED, from EIA) ----
# FRED series:
#   MGASUSGULF    Conventional Gasoline Prices: U.S. Gulf Coast, Regular        (from 2004-01)
#   MDFUELUSGULF  Ultra-Low-Sulfur No. 2 Diesel Fuel Prices: U.S. Gulf Coast    (from 2006-06)
#   MGASNYH       Conventional Gasoline Prices: New York Harbor, Regular        (from 2004-01)
#   MDFUELNYH     Ultra-Low-Sulfur No. 2 Diesel Fuel Prices: New York Harbor    (from 2006-06)
#   MHOILNYH      No. 2 Heating Oil Prices: New York Harbor                     (from 1986-06)
# The M* series are the monthly averages that EIA publishes of the daily D*
# series (DGASUSGULF, DDFUELUSGULF, DGASNYH, DDFUELNYH) and are used as they come.
# 1 US gallon = 0.003785411784 m3.
#
# Diesel before June 2006: EIA has no ULSD quotes before 2006 (the earlier spot
# was "No. 2 diesel low sulfur", which is not on FRED). For 2004-01 to 2006-05,
# NYH heating oil is scaled by the mean ULSD/heating oil ratio over the first 12
# months of overlap (2006-06 to 2007-05). Heating oil and No. 2 diesel are the
# same middle-distillate cut and differ only in sulfur content, so they move
# almost one for one. The result goes to the columns *_emp and fuente_gasoil_emp.
GAL_M3  <- 0.003785411784
REF_IDS <- c(nafta_usgulf="MGASUSGULF", gasoil_usgulf="MDFUELUSGULF",
             nafta_nyh="MGASNYH",       gasoil_nyh="MDFUELNYH",
             heating_nyh="MHOILNYH")
ref <- data.table(mes = seq(as.Date("2004-01-01"), as.Date("2024-12-01"), by="month"))
for (nm in names(REF_IDS)) {
  id <- REF_IDS[[nm]]
  U  <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=2004-01-01&coed=2024-12-31", id)
  f  <- fs::path(RAW, sprintf("%s_fred.csv", nm))
  dl(U, f)
  x <- fread(f, encoding="UTF-8", na.strings=c(".",""))      # FRED codes missing values as "."
  stopifnot(ncol(x)==2L, names(x)[2]==id)
  setnames(x, c("mes", paste0(nm, "_usd_gal")))
  x[, mes := mfloor(mes)]
  x <- x[, .(v = mean(get(paste0(nm,"_usd_gal")), na.rm=TRUE)), by=mes]   # in case the series comes at daily frequency
  x[!is.finite(v), v := NA_real_]
  setnames(x, "v", paste0(nm, "_usd_gal"))
  ref <- merge(ref, x, by="mes", all.x=TRUE)
}
# Splice USGC and NYH diesel with NYH heating oil for 2004-01 to 2006-05
for (mk in c("usgulf","nyh")) {
  g  <- paste0("gasoil_", mk, "_usd_gal")
  ini <- ref[!is.na(get(g)), min(mes)]
  ov  <- ref[!is.na(get(g)) & !is.na(heating_nyh_usd_gal)][order(mes)][1:12]   # first 12 months of overlap
  k   <- mean(ov[[g]] / ov$heating_nyh_usd_gal)
  stopifnot(is.finite(k), abs(k-1) < 0.15)     # the ULSD/heating oil ratio should be within 15% of 1
  ref[, paste0("gasoil_", mk, "_emp_usd_gal") :=
        fifelse(!is.na(get(g)), get(g), round(k * heating_nyh_usd_gal, 3))]
  cat(sprintf("gasoil_%s: ULSD from %s; ULSD/heating oil ratio (12m) = %.4f\n", mk, ini, k))
}
ref[, fuente_gasoil_emp := fifelse(!is.na(gasoil_usgulf_usd_gal), "ulsd_eia",
                           fifelse(!is.na(heating_nyh_usd_gal), "heating_oil_nyh_escalado", NA_character_))]
for (cn in grep("_usd_gal$", names(ref), value=TRUE))
  ref[, sub("_usd_gal$", "_usd_m3", cn) := round(get(cn) / GAL_M3, 2)]
ref_cols <- c("mes",
  "nafta_usgulf_usd_gal","nafta_usgulf_usd_m3","gasoil_usgulf_usd_gal","gasoil_usgulf_usd_m3",
  "nafta_nyh_usd_gal","nafta_nyh_usd_m3","gasoil_nyh_usd_gal","gasoil_nyh_usd_m3",
  "heating_nyh_usd_gal","heating_nyh_usd_m3",
  "gasoil_usgulf_emp_usd_gal","gasoil_usgulf_emp_usd_m3","gasoil_nyh_emp_usd_gal","gasoil_nyh_emp_usd_m3",
  "fuente_gasoil_emp")
ref <- ref[, ..ref_cols]

# 4. Official exchange rate A 3500, monthly average (datos.gob.ar mirror of BCRA) ----
U_TC <- paste0("https://apis.datos.gob.ar/series/api/series/?ids=175.1_DR_REFE500_0_0_25",
  "&format=csv&collapse=month&collapse_aggregation=avg&start_date=2004-01&end_date=2024-12&limit=5000")
dl(U_TC, fs::path(RAW, "tc_a3500_mensual.csv"))
tc <- fread(fs::path(RAW, "tc_a3500_mensual.csv"), encoding="UTF-8")
setnames(tc, c("mes","tc_a3500_prom"))
tc[, mes := as.Date(mes)]

# 5. Assemble 2004-01 to 2024-12 ----
out <- data.table(mes = seq(as.Date("2004-01-01"), as.Date("2024-12-01"), by="month"))
out <- Reduce(function(a,b) merge(a,b,by="mes",all.x=TRUE),
              list(out, ipc[, .(mes, ipc_empalmado = round(ipc_empalmado,3), fuente_ipc,
                                ipc_gba=gba, ipc_sanluis=sanluis, ipc_caba=caba, ipc_nacional=nacional)],
                   br, tc, ref))
out[, ipc_2004_100 := round(100 * ipc_empalmado / out[mes==as.Date("2004-01-01"), ipc_empalmado], 2)]
# CPI, Brent and exchange rate columns first, refined products at the end
setcolorder(out, c("mes","ipc_empalmado","fuente_ipc","ipc_gba","ipc_sanluis","ipc_caba","ipc_nacional",
                   "brent_usd_bbl","tc_a3500_prom","ipc_2004_100"))
stopifnot(nrow(out)==252L, !anyNA(out$ipc_empalmado), !anyNA(out$brent_usd_bbl), !anyNA(out$tc_a3500_prom),
          !anyNA(out$nafta_usgulf_usd_gal), !anyNA(out$gasoil_usgulf_emp_usd_gal),
          !anyNA(out[mes >= as.Date("2006-06-01"), gasoil_usgulf_usd_gal]))
fwrite(out, fs::path(DIR, "covar_series_nacionales.csv"), bom=TRUE)

# 6. Checks against known milestones ----
cat("[covar_series_nacionales.csv]", nrow(out), "months | columns:", paste(names(out), collapse=", "), "\n")
infl <- function(y) round(100*(out[mes==as.Date(sprintf("%d-12-01",y)), ipc_empalmado] /
                               out[mes==as.Date(sprintf("%d-12-01",y-1)), ipc_empalmado] - 1), 1)
cat("\nDecember-to-December inflation (compare with known figures):\n")
for (y in c(2005, 2007, 2010, 2014, 2016, 2019, 2023, 2024))
  cat(sprintf("  %d: %5.1f%%\n", y, infl(y)))
cat("\nsplices (m/m change around the splice month, should look normal):\n")
for (m in c("2005-12-01","2006-01-01","2016-12-01","2017-01-01")) {
  i <- which(out$mes==as.Date(m))
  cat(sprintf("  %s: %+.2f%%  [%s]\n", m, 100*(out$ipc_empalmado[i]/out$ipc_empalmado[i-1]-1), out$fuente_ipc[i]))
}
cat("\nBrent and exchange rate milestones:\n")
cat("  Brent Jul-2008 (peak):", out[mes==as.Date("2008-07-01"), brent_usd_bbl], "USD |",
    "Apr-2020 (COVID):", out[mes==as.Date("2020-04-01"), brent_usd_bbl], "USD\n")
cat("  FX Nov-2015:", out[mes==as.Date("2015-11-01"), tc_a3500_prom], "| Dec-2015 (capital controls lifted):",
    out[mes==as.Date("2015-12-01"), tc_a3500_prom], "\n")
cat("  FX Nov-2023:", out[mes==as.Date("2023-11-01"), tc_a3500_prom], "| Dec-2023 (devaluation):",
    out[mes==as.Date("2023-12-01"), tc_a3500_prom], "\n")

cat("\nrefined products (USD/gal): coverage and NA\n")
for (cn in grep("_usd_gal$", names(out), value=TRUE)) {
  ok <- out[!is.na(get(cn))]
  cat(sprintf("  %-28s %s to %s | NA: %3d | min %.3f  max %.3f\n", cn,
              format(min(ok$mes), "%Y-%m"), format(max(ok$mes), "%Y-%m"),
              sum(is.na(out[[cn]])), min(ok[[cn]]), max(ok[[cn]])))
}
pk <- function(cn, y) {
  s <- out[year(mes)==y]
  s[which.max(get(cn)), sprintf("%s (%.3f)", format(mes,"%Y-%m"), get(cn))]
}
cat("  USGC gasoline peak 2008:", pk("nafta_usgulf_usd_gal", 2008), "| peak 2022:", pk("nafta_usgulf_usd_gal", 2022), "\n")
cat("  USGC diesel peak 2008:", pk("gasoil_usgulf_usd_gal", 2008), "| peak 2022:", pk("gasoil_usgulf_usd_gal", 2022), "\n")

# External check: USGC gasoline against the import unit value of Nafta Grado 3
# in SESCO. Runs only if sesco_downstream.R has already written its trade file.
f_ce <- fs::path(DIR, "covar_sesco_comercio_ext.csv")
if (fs::file_exists(f_ce)) {
  ce <- fread(f_ce, encoding="UTF-8")
  ce <- ce[tipo=="Importación" & grepl("^Nafta Grado 3", producto),
           .(mes = as.Date(sprintf("%d-%02d-01", anio, mes)), vu_imp_nafta_usd_m3 = valor_unitario)]
  cmp <- merge(ce, out[, .(mes, nafta_usgulf_usd_m3, brent_usd_bbl)], by="mes")
  cmp <- cmp[is.finite(vu_imp_nafta_usd_m3) & vu_imp_nafta_usd_m3 > 0]
  cat(sprintf("\nUSGC gasoline (USD/m3) vs SESCO import unit value of Nafta Grado 3 (%d months, %s..%s):\n",
              nrow(cmp), format(min(cmp$mes),"%Y-%m"), format(max(cmp$mes),"%Y-%m")))
  cat(sprintf("  SESCO/USGC ratio: mean %.3f | median %.3f | p10 %.3f | p90 %.3f\n",
              cmp[, mean(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3)], cmp[, median(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3)],
              cmp[, quantile(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3, .1)], cmp[, quantile(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3, .9)]))
  cat(sprintf("  corr(levels) = %.3f | corr(logs) = %.3f | corr with Brent = %.3f\n",
              cmp[, cor(vu_imp_nafta_usd_m3, nafta_usgulf_usd_m3)], cmp[, cor(log(vu_imp_nafta_usd_m3), log(nafta_usgulf_usd_m3))],
              cmp[, cor(vu_imp_nafta_usd_m3, brent_usd_bbl)]))
}

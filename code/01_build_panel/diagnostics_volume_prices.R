# diagnostics_volume_prices.R
# Diagnostics on the cleaned panel: distributions of volume and of the three
# price variables, outlier counts, figures. The panel itself is not modified.
#
# Input:  eess_all_cleaned3_cut.rds
# Output: figures (.png) and workbooks of summary tables (.xlsx) in the folders
#         analisis_volumen_cleaned3_cut, analisis_precios_cleaned3_cut and
#         analisis_precio_sin_impuestos under DIR_INTERIM

library(data.table)
library(fs)
library(openxlsx)
library(ggplot2)

source("code/00_config.R")


# Volume ----

## Load the panel ----

DIR_DATASETS <- DIR_INTERIM

if (!exists("eess_all_cleaned3_cut")) {
  eess_all_cleaned3_cut <- readRDS(fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds"))
}

setDT(eess_all_cleaned3_cut)

# Output folder for this part
DIR_OUT_VOL <- fs::path(DIR_DATASETS, "analisis_volumen_cleaned3_cut")
fs::dir_create(DIR_OUT_VOL)

cat("Panel loaded.\n")
cat("Rows:", nrow(eess_all_cleaned3_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned3_cut), "\n")

## Numeric volume ----

# volumen is already numeric in this panel, and rows with volume below 1e-3 were
# dropped when it was built. The counts of missing-value markers, parse
# failures, zeros and negatives in this part are a guard and should all be zero.

# Character and numeric copies; the original column is kept
vol_chr <- trimws(as.character(eess_all_cleaned3_cut$volumen))

eess_all_cleaned3_cut[, volumen_chr := vol_chr]
eess_all_cleaned3_cut[, volumen_num := suppressWarnings(as.numeric(volumen_chr))]

# Rebuild anio (year) from the period variable if it is missing
if (!"anio" %in% names(eess_all_cleaned3_cut)) {
  if ("periodo" %in% names(eess_all_cleaned3_cut)) {
    eess_all_cleaned3_cut[, anio := as.integer(substr(periodo, 1, 4))]
  } else if ("periodo_dt" %in% names(eess_all_cleaned3_cut)) {
    eess_all_cleaned3_cut[, anio := as.integer(format(periodo_dt, "%Y"))]
  }
}

## Raw diagnostics ----

diag_crudo <- data.table(
  n_total = nrow(eess_all_cleaned3_cut),
  clase_original = paste(class(eess_all_cleaned3_cut$volumen), collapse = " | "),
  n_na_original = sum(is.na(eess_all_cleaned3_cut$volumen)),
  n_blank = sum(!is.na(vol_chr) & vol_chr == ""),
  n_ND = sum(vol_chr == "ND", na.rm = TRUE),
  n_N_D = sum(vol_chr == "N/D", na.rm = TRUE),
  n_guion = sum(vol_chr == "-", na.rm = TRUE),
  n_con_coma = sum(grepl(",", vol_chr, fixed = TRUE), na.rm = TRUE),
  n_con_punto = sum(grepl(".", vol_chr, fixed = TRUE), na.rm = TRUE),
  n_con_espacios = sum(grepl(" ", vol_chr, fixed = TRUE), na.rm = TRUE)
)

print(diag_crudo)

cat("\nFirst unique raw values:\n")
print(head(unique(vol_chr), 50))

cat("\nMost frequent raw values:\n")
print(head(sort(table(vol_chr), decreasing = TRUE), 50))

## Parsing to numeric ----

# Parse failures other than empty strings and missing-value markers
vol_fail <- eess_all_cleaned3_cut[
  !is.na(volumen_chr) &
    volumen_chr != "" &
    !(volumen_chr %in% c("N/D", "ND", "-", "NA", "NULL")) &
    is.na(volumen_num),
  .(volumen_chr)
]

diag_parseo <- data.table(
  n_total = nrow(eess_all_cleaned3_cut),
  n_na_num = sum(is.na(eess_all_cleaned3_cut$volumen_num)),
  n_no_parseados_no_triviales = nrow(vol_fail),
  n_cero = sum(eess_all_cleaned3_cut$volumen_num == 0, na.rm = TRUE),
  n_negativos = sum(eess_all_cleaned3_cut$volumen_num < 0, na.rm = TRUE),
  n_positivos = sum(eess_all_cleaned3_cut$volumen_num > 0, na.rm = TRUE)
)

print(diag_parseo)

if (nrow(vol_fail) > 0) {
  cat("\nExamples of values that failed to parse:\n")
  print(head(unique(vol_fail$volumen_chr), 100))
}

## Overall distribution ----

resumen_global <- data.table(
  n = sum(!is.na(eess_all_cleaned3_cut$volumen_num)),
  media = mean(eess_all_cleaned3_cut$volumen_num, na.rm = TRUE),
  mediana = median(eess_all_cleaned3_cut$volumen_num, na.rm = TRUE),
  sd = sd(eess_all_cleaned3_cut$volumen_num, na.rm = TRUE),
  min = min(eess_all_cleaned3_cut$volumen_num, na.rm = TRUE),
  max = max(eess_all_cleaned3_cut$volumen_num, na.rm = TRUE)
)

print(resumen_global)

quant_global <- data.table(
  prob = c(0, 0.0001, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 0.9999, 1),
  q = as.numeric(quantile(
    eess_all_cleaned3_cut$volumen_num,
    probs = c(0, 0.0001, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 0.9999, 1),
    na.rm = TRUE
  ))
)

print(quant_global)

## Very small volumes ----

# Bands for volumes at or below 1. With volume >= 1e-3 in the panel, the bands
# up to 1e-4 should be empty and (1e-4,1e-3] can only hold values of exactly
# 1e-3.
eess_all_cleaned3_cut[
  , banda_small := fifelse(
    is.na(volumen_num), NA_character_,
    fifelse(volumen_num == 0, "0",
            fifelse(volumen_num < 0, "<0",
                    fifelse(volumen_num <= 1e-5, "(0,1e-5]",
                            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
                                    fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
                                            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
                                                    fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                                                            fifelse(volumen_num <= 1, "(1e-1,1]",
                                                                    ">1")))))))))
]

small_global <- eess_all_cleaned3_cut[, .N, by = banda_small][order(banda_small)]
print(small_global)

small_by_year <- eess_all_cleaned3_cut[
  !is.na(anio),
  .N,
  by = .(anio, banda_small)
][order(anio, banda_small)]

print(small_by_year)

small_by_prod <- eess_all_cleaned3_cut[
  volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(producto, banda_small)
][order(producto, banda_small)]

print(small_by_prod)

tiny_examples <- eess_all_cleaned3_cut[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .(periodo, anio, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num)
][order(anio, producto, volumen_num)]

cat("\nExamples of tiny volumes:\n")
print(tiny_examples[1:100])

## Summary by year ----

res_anio <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(volumen_num)),
    share_na = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    n_neg = sum(volumen_num < 0, na.rm = TRUE),
    media = mean(volumen_num, na.rm = TRUE),
    p1 = quantile(volumen_num, 0.01, na.rm = TRUE),
    p5 = quantile(volumen_num, 0.05, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE),
    suma_vol = sum(volumen_num, na.rm = TRUE)
  ),
  by = anio
][order(anio)]

print(res_anio)

## Summary by product ----

res_producto <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(volumen_num)),
    share_na = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    n_neg = sum(volumen_num < 0, na.rm = TRUE),
    media = mean(volumen_num, na.rm = TRUE),
    p1 = quantile(volumen_num, 0.01, na.rm = TRUE),
    p5 = quantile(volumen_num, 0.05, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE),
    suma_vol = sum(volumen_num, na.rm = TRUE)
  ),
  by = producto
][order(-n)]

print(res_producto)

## Summary by product x sales channel ----

res_prod_canal <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(volumen_num)),
    share_na = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, canal_de_comercializacion)
][order(producto, -n)]

print(res_prod_canal[1:100])

## Summary by product x business type ----

res_prod_tipo <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(volumen_num)),
    share_na = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, tipo_negocio)
][order(producto, -n)]

print(res_prod_tipo[1:100])

## Summary by year x product ----

res_anio_producto <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(volumen_num)),
    share_na = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE),
    suma_vol = sum(volumen_num, na.rm = TRUE)
  ),
  by = .(anio, producto)
][order(anio, producto)]

print(res_anio_producto)

## Largest and smallest volumes ----

top_max <- eess_all_cleaned3_cut[
  !is.na(volumen_num)
][order(-volumen_num),
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
    volumen, volumen_num, source_file)
][1:200]

print(top_max)

top_min_pos <- eess_all_cleaned3_cut[
  !is.na(volumen_num) & volumen_num > 0
][order(volumen_num),
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
    volumen, volumen_num, source_file)
][1:200]

print(top_min_pos)

negativos <- eess_all_cleaned3_cut[
  !is.na(volumen_num) & volumen_num < 0,
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
    volumen, volumen_num, source_file)
][order(volumen_num)]

print(negativos)

## Outliers by product ----

# Product-level cutoffs. A row is flagged if it is above the product's p99.9,
# above its p99.99, or more than 1,000 times its median.
cutoffs_prod <- eess_all_cleaned3_cut[
  !is.na(volumen_num),
  .(
    n = .N,
    mediana_prod = median(volumen_num, na.rm = TRUE),
    p99_prod = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999_prod = quantile(volumen_num, 0.999, na.rm = TRUE),
    p9999_prod = quantile(volumen_num, 0.9999, na.rm = TRUE)
  ),
  by = producto
]

eess_all_cleaned3_cut <- cutoffs_prod[eess_all_cleaned3_cut, on = "producto"]

eess_all_cleaned3_cut[, flag_out_p999 := !is.na(volumen_num) & volumen_num > p999_prod]
eess_all_cleaned3_cut[, flag_out_p9999 := !is.na(volumen_num) & volumen_num > p9999_prod]
eess_all_cleaned3_cut[, flag_out_ratio := !is.na(volumen_num) & !is.na(mediana_prod) & mediana_prod > 0 &
                        volumen_num > 1000 * mediana_prod]

outliers_prod_res <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_out_p999 = sum(flag_out_p999, na.rm = TRUE),
    n_out_p9999 = sum(flag_out_p9999, na.rm = TRUE),
    n_out_ratio = sum(flag_out_ratio, na.rm = TRUE)
  ),
  by = producto
][order(-n_out_p9999, -n_out_ratio)]

print(outliers_prod_res)

outliers_rows <- eess_all_cleaned3_cut[
  flag_out_p9999 == TRUE | flag_out_ratio == TRUE,
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
    volumen, volumen_num, mediana_prod, p999_prod, p9999_prod, source_file)
][order(producto, -volumen_num)]

print(outliers_rows[1:300])

## Stations that concentrate the outliers ----

# Stations are identified by nro_inscripcion (registration number)
outliers_estacion <- eess_all_cleaned3_cut[
  flag_out_p9999 == TRUE | flag_out_ratio == TRUE,
  .(
    n_outliers = .N,
    productos = paste(sort(unique(producto)), collapse = " | "),
    canales = paste(sort(unique(canal_de_comercializacion)), collapse = " | "),
    min_anio = min(anio, na.rm = TRUE),
    max_anio = max(anio, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_outliers, -max_vol)]

print(outliers_estacion[1:100])

## Monthly totals ----

if ("periodo_dt" %in% names(eess_all_cleaned3_cut)) {
  mensual_total <- eess_all_cleaned3_cut[
    , .(
      volumen_total = sum(volumen_num, na.rm = TRUE),
      volumen_mediano = median(volumen_num, na.rm = TRUE),
      n = .N,
      n_na = sum(is.na(volumen_num))
    ),
    by = periodo_dt
  ][order(periodo_dt)]

  print(head(mensual_total, 24))
  print(tail(mensual_total, 24))
}

mensual_producto <- eess_all_cleaned3_cut[
  , .(
    volumen_total = sum(volumen_num, na.rm = TRUE),
    n = .N
  ),
  by = .(periodo_dt, producto)
][order(periodo_dt, producto)]

print(head(mensual_producto, 50))

## Zero volumes and the no_movimientos flag ----

# no_movimientos (SI/NO) marks records reported with no activity
if ("no_movimientos" %in% names(eess_all_cleaned3_cut)) {
  chequeo_nomov <- eess_all_cleaned3_cut[
    , .(
      n = .N,
      n_cero = sum(volumen_num == 0, na.rm = TRUE),
      share_cero = mean(volumen_num == 0, na.rm = TRUE)
    ),
    by = no_movimientos
  ][order(-n)]

  print(chequeo_nomov)
}

## Figures ----

# Full distribution
g1 <- ggplot(
  eess_all_cleaned3_cut[!is.na(volumen_num) & is.finite(volumen_num)],
  aes(x = volumen_num)
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución completa de volumen",
    x = "volumen_num",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_VOL, "hist_volumen_completo.png"), g1, width = 10, height = 6)

# Truncated at the overall p99
p99_global <- quantile(eess_all_cleaned3_cut$volumen_num, 0.99, na.rm = TRUE)

g2 <- ggplot(
  eess_all_cleaned3_cut[!is.na(volumen_num) & volumen_num <= p99_global],
  aes(x = volumen_num)
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de volumen truncada al p99",
    x = "volumen_num",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_VOL, "hist_volumen_p99.png"), g2, width = 10, height = 6)

# log(1 + volume)
g3 <- ggplot(
  eess_all_cleaned3_cut[!is.na(volumen_num) & volumen_num >= 0],
  aes(x = log1p(volumen_num))
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de log(1 + volumen)",
    x = "log(1 + volumen_num)",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_VOL, "hist_log1p_volumen.png"), g3, width = 10, height = 6)

# Monthly total volume
if ("periodo_dt" %in% names(eess_all_cleaned3_cut)) {
  g4 <- ggplot(mensual_total, aes(x = periodo_dt, y = volumen_total)) +
    geom_line() +
    labs(
      title = "Volumen total mensual",
      x = "Período",
      y = "Suma mensual de volumen"
    )

  ggsave(fs::path(DIR_OUT_VOL, "serie_volumen_total_mensual.png"), g4, width = 11, height = 6)
}

## Export tables to Excel ----

wb <- createWorkbook()

addWorksheet(wb, "diag_crudo")
writeData(wb, "diag_crudo", diag_crudo)

addWorksheet(wb, "diag_parseo")
writeData(wb, "diag_parseo", diag_parseo)

addWorksheet(wb, "resumen_global")
writeData(wb, "resumen_global", resumen_global)

addWorksheet(wb, "quant_global")
writeData(wb, "quant_global", quant_global)

addWorksheet(wb, "small_global")
writeData(wb, "small_global", small_global)

addWorksheet(wb, "small_by_year")
writeData(wb, "small_by_year", small_by_year)

addWorksheet(wb, "res_anio")
writeData(wb, "res_anio", res_anio)

addWorksheet(wb, "res_producto")
writeData(wb, "res_producto", res_producto)

addWorksheet(wb, "res_prod_canal")
writeData(wb, "res_prod_canal", res_prod_canal)

addWorksheet(wb, "res_prod_tipo")
writeData(wb, "res_prod_tipo", res_prod_tipo)

addWorksheet(wb, "res_anio_producto")
writeData(wb, "res_anio_producto", res_anio_producto)

addWorksheet(wb, "top_max")
writeData(wb, "top_max", top_max)

addWorksheet(wb, "top_min_pos")
writeData(wb, "top_min_pos", top_min_pos)

addWorksheet(wb, "negativos")
writeData(wb, "negativos", negativos)

addWorksheet(wb, "outliers_prod_res")
writeData(wb, "outliers_prod_res", outliers_prod_res)

addWorksheet(wb, "outliers_rows")
writeData(wb, "outliers_rows", outliers_rows)

addWorksheet(wb, "outliers_estacion")
writeData(wb, "outliers_estacion", outliers_estacion)

if (exists("mensual_total")) {
  addWorksheet(wb, "mensual_total")
  writeData(wb, "mensual_total", mensual_total)
}

addWorksheet(wb, "mensual_producto")
writeData(wb, "mensual_producto", mensual_producto)

saveWorkbook(
  wb,
  fs::path(DIR_OUT_VOL, "analisis_exhaustivo_volumen_cleaned3_cut.xlsx"),
  overwrite = TRUE
)

cat("\nDone. Outputs saved in:\n")
cat(DIR_OUT_VOL, "\n")

# Quick look at the cleaned volume column with base graphics
x <- eess_all_cleaned3_cut$volumen
summary(x)

hist(x, breaks = 100, main = "Volumen: distribución completa", xlab = "volumen")


# Price variables ----

# This part and the next repeat the setup (libraries, paths, reload of the
# panel), so each can be run on its own once the config has been sourced.

library(data.table)
library(fs)
library(openxlsx)
library(ggplot2)

## Load the panel ----

DIR_DATASETS <- DIR_INTERIM
DIR_OUT_PRICE <- fs::path(DIR_DATASETS, "analisis_precios_cleaned3_cut")
fs::dir_create(DIR_OUT_PRICE)

# Drop the in-memory panel (the volume diagnostics added columns to it) and
# leftovers from an earlier run. rm() only warns if an object does not exist.
rm(list = c("eess_all_cleaned3_cut", "eess_price", "res_precios"))
gc()

# Reload the panel as saved on disk
base_clean <- readRDS(fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds"))
setDT(base_clean)

# Working copy for the price diagnostics
eess_price <- copy(base_clean)

# Drop the loaded object so that it cannot be modified by accident
rm(base_clean)
gc()

cat("Panel loaded.\n")
cat("Rows:", nrow(eess_price), "\n")
cat("Columns:", ncol(eess_price), "\n")

## Auxiliary variables ----

# Year, rebuilt from the period variable if it is missing
if (!"anio" %in% names(eess_price)) {
  if ("periodo" %in% names(eess_price)) {
    eess_price[, anio := as.integer(substr(periodo, 1, 4))]
  } else if ("periodo_dt" %in% names(eess_price)) {
    eess_price[, anio := as.integer(format(periodo_dt, "%Y"))]
  } else {
    stop("Found neither 'anio' nor a period variable to rebuild it from.")
  }
}

# Numeric volume, used to weight the mean price
if (!"volumen_num" %in% names(eess_price) && "volumen" %in% names(eess_price)) {
  eess_price[, volumen_num := suppressWarnings(as.numeric(as.character(volumen)))]
}

# Price variables present in the panel: pump price, price with taxes and price
# without taxes. All three are still stored as text at this stage. Zero prices
# are written as "0E-7", which as.numeric() reads as 0; they are frequent in
# precio_surtidor (about 750,000 rows) and rare in the other two.
PRICE_VARS <- intersect(
  c("precio_surtidor", "precio_con_impuestos", "precio_sin_impuestos"),
  names(eess_price)
)

print(PRICE_VARS)

## Helpers ----

# Quantile, min, max and weighted mean that drop NA and non-finite values (and
# non-positive weights) and return NA when nothing is left
weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

q_safe <- function(x, p) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, probs = p, na.rm = TRUE))
}

min_safe <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

max_safe <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

## Diagnostics for one price variable ----

# Runs every diagnostic for price_var on a copy of dt, saves the figures and the
# workbook analisis_<price_var>.xlsx in out_dir, and returns the main tables
# invisibly
analizar_precio <- function(dt, price_var, out_dir) {

  cat("\n============================================================\n")
  cat("Analyzing:", price_var, "\n")
  cat("============================================================\n")

  dt <- copy(dt)

  price_chr <- trimws(as.character(dt[[price_var]]))
  price_num_var <- paste0(price_var, "_num")
  dt[, (price_num_var) := suppressWarnings(as.numeric(price_chr))]

  # Raw diagnostics
  diag_crudo <- data.table(
    variable = price_var,
    n_total = nrow(dt),
    clase_original = paste(class(dt[[price_var]]), collapse = " | "),
    n_na_original = sum(is.na(dt[[price_var]])),
    n_blank = sum(!is.na(price_chr) & price_chr == ""),
    n_ND = sum(price_chr == "ND", na.rm = TRUE),
    n_N_D = sum(price_chr == "N/D", na.rm = TRUE),
    n_guion = sum(price_chr == "-", na.rm = TRUE),
    n_con_coma = sum(grepl(",", price_chr, fixed = TRUE), na.rm = TRUE),
    n_con_punto = sum(grepl(".", price_chr, fixed = TRUE), na.rm = TRUE),
    n_con_espacios = sum(grepl(" ", price_chr, fixed = TRUE), na.rm = TRUE)
  )

  print(diag_crudo)

  cat("\nMost frequent raw values:\n")
  print(head(sort(table(price_chr), decreasing = TRUE), 50))

  # Parsing to numeric
  parse_fail <- dt[
    !is.na(price_chr) &
      price_chr != "" &
      !(price_chr %in% c("N/D", "ND", "-", "NA", "NULL")) &
      is.na(get(price_num_var)),
    .(valor = price_chr)
  ]

  diag_parseo <- data.table(
    variable = price_var,
    n_total = nrow(dt),
    n_na_num = sum(is.na(dt[[price_num_var]])),
    n_no_parseados_no_triviales = nrow(parse_fail),
    n_cero = sum(dt[[price_num_var]] == 0, na.rm = TRUE),
    n_negativos = sum(dt[[price_num_var]] < 0, na.rm = TRUE),
    n_positivos = sum(dt[[price_num_var]] > 0, na.rm = TRUE)
  )

  print(diag_parseo)

  if (nrow(parse_fail) > 0) {
    cat("\nExamples of values that failed to parse:\n")
    print(head(unique(parse_fail$valor), 100))
  }

  # Overall summary and quantiles
  resumen_global <- data.table(
    variable = price_var,
    n = sum(!is.na(dt[[price_num_var]])),
    media = mean(dt[[price_num_var]], na.rm = TRUE),
    mediana = median(dt[[price_num_var]], na.rm = TRUE),
    sd = sd(dt[[price_num_var]], na.rm = TRUE),
    min = min_safe(dt[[price_num_var]]),
    max = max_safe(dt[[price_num_var]])
  )

  quant_global <- data.table(
    variable = price_var,
    prob = c(0, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 1),
    q = c(
      q_safe(dt[[price_num_var]], 0),
      q_safe(dt[[price_num_var]], 0.001),
      q_safe(dt[[price_num_var]], 0.005),
      q_safe(dt[[price_num_var]], 0.01),
      q_safe(dt[[price_num_var]], 0.05),
      q_safe(dt[[price_num_var]], 0.10),
      q_safe(dt[[price_num_var]], 0.25),
      q_safe(dt[[price_num_var]], 0.50),
      q_safe(dt[[price_num_var]], 0.75),
      q_safe(dt[[price_num_var]], 0.90),
      q_safe(dt[[price_num_var]], 0.95),
      q_safe(dt[[price_num_var]], 0.99),
      q_safe(dt[[price_num_var]], 0.999),
      q_safe(dt[[price_num_var]], 1)
    )
  )

  print(resumen_global)
  print(quant_global)

  # Summary by year
  res_anio <- dt[
    , .(
      n = .N,
      n_na = sum(is.na(get(price_num_var))),
      share_na = mean(is.na(get(price_num_var))),
      n_cero = sum(get(price_num_var) == 0, na.rm = TRUE),
      share_cero = mean(get(price_num_var) == 0, na.rm = TRUE),
      n_neg = sum(get(price_num_var) < 0, na.rm = TRUE),
      media = mean(get(price_num_var), na.rm = TRUE),
      mediana = median(get(price_num_var), na.rm = TRUE),
      sd = sd(get(price_num_var), na.rm = TRUE),
      p1 = q_safe(get(price_num_var), 0.01),
      p5 = q_safe(get(price_num_var), 0.05),
      p95 = q_safe(get(price_num_var), 0.95),
      p99 = q_safe(get(price_num_var), 0.99),
      min_precio = min_safe(get(price_num_var)),
      max_precio = max_safe(get(price_num_var)),
      media_pond_vol = if ("volumen_num" %in% names(dt)) weighted_mean_safe(get(price_num_var), volumen_num) else NA_real_
    ),
    by = anio
  ][order(anio)]

  print(res_anio)

  # Summary by product
  res_producto <- dt[
    , .(
      n = .N,
      n_na = sum(is.na(get(price_num_var))),
      share_na = mean(is.na(get(price_num_var))),
      n_cero = sum(get(price_num_var) == 0, na.rm = TRUE),
      share_cero = mean(get(price_num_var) == 0, na.rm = TRUE),
      n_neg = sum(get(price_num_var) < 0, na.rm = TRUE),
      media = mean(get(price_num_var), na.rm = TRUE),
      mediana = median(get(price_num_var), na.rm = TRUE),
      sd = sd(get(price_num_var), na.rm = TRUE),
      p1 = q_safe(get(price_num_var), 0.01),
      p5 = q_safe(get(price_num_var), 0.05),
      p95 = q_safe(get(price_num_var), 0.95),
      p99 = q_safe(get(price_num_var), 0.99),
      min_precio = min_safe(get(price_num_var)),
      max_precio = max_safe(get(price_num_var)),
      media_pond_vol = if ("volumen_num" %in% names(dt)) weighted_mean_safe(get(price_num_var), volumen_num) else NA_real_
    ),
    by = producto
  ][order(-n)]

  print(res_producto)

  # Summary by year x product
  res_anio_producto <- dt[
    , .(
      n = .N,
      n_na = sum(is.na(get(price_num_var))),
      share_na = mean(is.na(get(price_num_var))),
      n_cero = sum(get(price_num_var) == 0, na.rm = TRUE),
      share_cero = mean(get(price_num_var) == 0, na.rm = TRUE),
      n_neg = sum(get(price_num_var) < 0, na.rm = TRUE),
      media = mean(get(price_num_var), na.rm = TRUE),
      mediana = median(get(price_num_var), na.rm = TRUE),
      p1 = q_safe(get(price_num_var), 0.01),
      p5 = q_safe(get(price_num_var), 0.05),
      p95 = q_safe(get(price_num_var), 0.95),
      p99 = q_safe(get(price_num_var), 0.99),
      min_precio = min_safe(get(price_num_var)),
      max_precio = max_safe(get(price_num_var)),
      media_pond_vol = if ("volumen_num" %in% names(dt)) weighted_mean_safe(get(price_num_var), volumen_num) else NA_real_
    ),
    by = .(anio, producto)
  ][order(anio, producto)]

  print(res_anio_producto)

  # Summary by product x sales channel
  res_prod_canal <- dt[
    , .(
      n = .N,
      n_na = sum(is.na(get(price_num_var))),
      share_na = mean(is.na(get(price_num_var))),
      n_cero = sum(get(price_num_var) == 0, na.rm = TRUE),
      share_cero = mean(get(price_num_var) == 0, na.rm = TRUE),
      mediana = median(get(price_num_var), na.rm = TRUE),
      p95 = q_safe(get(price_num_var), 0.95),
      p99 = q_safe(get(price_num_var), 0.99),
      min_precio = min_safe(get(price_num_var)),
      max_precio = max_safe(get(price_num_var))
    ),
    by = .(producto, canal_de_comercializacion)
  ][order(producto, -n)]

  print(res_prod_canal[1:100])

  # Highest prices, lowest positive prices and negative prices
  top_max <- dt[
    !is.na(get(price_num_var))
  ][order(-get(price_num_var)),
    .(
      periodo, anio, nro_inscripcion, producto, tipo_negocio,
      canal_de_comercializacion, valor_precio = get(price_num_var),
      volumen = if ("volumen_num" %in% names(dt)) volumen_num else NA_real_,
      source_file
    )
  ][1:300]

  top_min_pos <- dt[
    !is.na(get(price_num_var)) & get(price_num_var) > 0
  ][order(get(price_num_var)),
    .(
      periodo, anio, nro_inscripcion, producto, tipo_negocio,
      canal_de_comercializacion, valor_precio = get(price_num_var),
      volumen = if ("volumen_num" %in% names(dt)) volumen_num else NA_real_,
      source_file
    )
  ][1:300]

  negativos <- dt[
    !is.na(get(price_num_var)) & get(price_num_var) < 0,
    .(
      periodo, anio, nro_inscripcion, producto, tipo_negocio,
      canal_de_comercializacion, valor_precio = get(price_num_var),
      volumen = if ("volumen_num" %in% names(dt)) volumen_num else NA_real_,
      source_file
    )
  ][order(valor_precio)]

  print(top_max[1:20])
  print(top_min_pos[1:20])
  print(negativos)

  # Outliers against the pooled distribution: 1.5 x IQR fences, outside p1-p99,
  # above p99.9. Prices are nominal (the median pre-tax price goes from about 1
  # in 2004 to about 830 in 2024), so the pooled flags mostly pick up the last
  # years; the flags within year and within year x product are the informative
  # ones.
  q25_g <- q_safe(dt[[price_num_var]], 0.25)
  q75_g <- q_safe(dt[[price_num_var]], 0.75)
  iqr_g <- q75_g - q25_g
  p1_g <- q_safe(dt[[price_num_var]], 0.01)
  p99_g <- q_safe(dt[[price_num_var]], 0.99)
  p999_g <- q_safe(dt[[price_num_var]], 0.999)

  dt[, flag_out_global_iqr := get(price_num_var) < (q25_g - 1.5 * iqr_g) |
       get(price_num_var) > (q75_g + 1.5 * iqr_g)]

  dt[, flag_out_global_p99 := get(price_num_var) < p1_g | get(price_num_var) > p99_g]
  dt[, flag_out_global_p999 := get(price_num_var) > p999_g]

  out_global_res <- dt[, .(
    n_total = .N,
    n_out_iqr = sum(flag_out_global_iqr, na.rm = TRUE),
    n_out_p99 = sum(flag_out_global_p99, na.rm = TRUE),
    n_out_p999 = sum(flag_out_global_p999, na.rm = TRUE)
  )]

  print(out_global_res)

  # Outliers within year
  cut_anio <- dt[
    !is.na(get(price_num_var)),
    .(
      n = .N,
      q25 = q_safe(get(price_num_var), 0.25),
      q75 = q_safe(get(price_num_var), 0.75),
      p1 = q_safe(get(price_num_var), 0.01),
      p99 = q_safe(get(price_num_var), 0.99),
      p999 = q_safe(get(price_num_var), 0.999),
      mediana = median(get(price_num_var), na.rm = TRUE)
    ),
    by = anio
  ]

  cut_anio[, iqr := q75 - q25]

  dt <- cut_anio[dt, on = "anio"]

  dt[, flag_out_anio_iqr := get(price_num_var) < (q25 - 1.5 * iqr) |
       get(price_num_var) > (q75 + 1.5 * iqr)]

  dt[, flag_out_anio_p99 := get(price_num_var) < p1 | get(price_num_var) > p99]
  dt[, flag_out_anio_p999 := get(price_num_var) > p999]

  out_anio_res <- dt[
    , .(
      n = .N,
      n_out_iqr = sum(flag_out_anio_iqr, na.rm = TRUE),
      n_out_p99 = sum(flag_out_anio_p99, na.rm = TRUE),
      n_out_p999 = sum(flag_out_anio_p999, na.rm = TRUE)
    ),
    by = anio
  ][order(anio)]

  print(out_anio_res)

  # Outliers within year x product; cells with fewer than 20 observations are
  # not flagged
  cut_anio_prod <- dt[
    !is.na(get(price_num_var)),
    .(
      n_gp = .N,
      q25_gp = q_safe(get(price_num_var), 0.25),
      q75_gp = q_safe(get(price_num_var), 0.75),
      p1_gp = q_safe(get(price_num_var), 0.01),
      p99_gp = q_safe(get(price_num_var), 0.99),
      p999_gp = q_safe(get(price_num_var), 0.999),
      mediana_gp = median(get(price_num_var), na.rm = TRUE)
    ),
    by = .(anio, producto)
  ]

  cut_anio_prod[, iqr_gp := q75_gp - q25_gp]

  dt <- cut_anio_prod[dt, on = .(anio, producto)]

  dt[, flag_out_anio_prod_iqr :=
       n_gp >= 20 &
       (get(price_num_var) < (q25_gp - 1.5 * iqr_gp) |
          get(price_num_var) > (q75_gp + 1.5 * iqr_gp))]

  dt[, flag_out_anio_prod_p99 :=
       n_gp >= 20 &
       (get(price_num_var) < p1_gp | get(price_num_var) > p99_gp)]

  dt[, flag_out_anio_prod_p999 :=
       n_gp >= 20 &
       get(price_num_var) > p999_gp]

  out_anio_prod_res <- dt[
    , .(
      n = .N,
      n_out_iqr = sum(flag_out_anio_prod_iqr, na.rm = TRUE),
      n_out_p99 = sum(flag_out_anio_prod_p99, na.rm = TRUE),
      n_out_p999 = sum(flag_out_anio_prod_p999, na.rm = TRUE)
    ),
    by = .(anio, producto)
  ][order(anio, producto)]

  print(out_anio_prod_res)

  # Rows flagged within year x product (p99.9 or IQR rule)
  outlier_rows_anio_prod <- dt[
    flag_out_anio_prod_p999 == TRUE | flag_out_anio_prod_iqr == TRUE,
    .(
      periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
      valor_precio = get(price_num_var),
      volumen = if ("volumen_num" %in% names(dt)) volumen_num else NA_real_,
      mediana_gp, p99_gp, p999_gp, q25_gp, q75_gp,
      source_file
    )
  ][order(anio, producto, -valor_precio)]

  print(outlier_rows_anio_prod[1:300])

  # Stations with the most flagged rows
  outliers_estacion <- dt[
    flag_out_anio_prod_p999 == TRUE | flag_out_anio_prod_iqr == TRUE,
    .(
      n_outliers = .N,
      productos = paste(sort(unique(producto)), collapse = " | "),
      min_anio = min(anio, na.rm = TRUE),
      max_anio = max(anio, na.rm = TRUE),
      max_precio = max(get(price_num_var), na.rm = TRUE)
    ),
    by = nro_inscripcion
  ][order(-n_outliers, -max_precio)]

  print(outliers_estacion[1:100])

  # Monthly series
  if ("periodo_dt" %in% names(dt)) {
    res_mes <- dt[
      , .(
        n = .N,
        n_na = sum(is.na(get(price_num_var))),
        media = mean(get(price_num_var), na.rm = TRUE),
        mediana = median(get(price_num_var), na.rm = TRUE),
        p5 = q_safe(get(price_num_var), 0.05),
        p95 = q_safe(get(price_num_var), 0.95),
        media_pond_vol = if ("volumen_num" %in% names(dt)) weighted_mean_safe(get(price_num_var), volumen_num) else NA_real_
      ),
      by = periodo_dt
    ][order(periodo_dt)]
  } else {
    res_mes <- data.table()
  }

  print(head(res_mes, 24))
  print(tail(res_mes, 24))

  # Figures
  safe_name <- gsub("[^a-zA-Z0-9_]+", "_", price_var)

  g1 <- ggplot(
    dt[!is.na(get(price_num_var)) & is.finite(get(price_num_var))],
    aes(x = get(price_num_var))
  ) +
    geom_histogram(bins = 100) +
    labs(
      title = paste("Distribución completa -", price_var),
      x = price_var,
      y = "Frecuencia"
    )

  ggsave(fs::path(out_dir, paste0("hist_", safe_name, "_completo.png")), g1, width = 10, height = 6)

  g2 <- ggplot(
    dt[!is.na(get(price_num_var)) & get(price_num_var) <= p99_g],
    aes(x = get(price_num_var))
  ) +
    geom_histogram(bins = 100) +
    labs(
      title = paste("Distribución truncada al p99 -", price_var),
      x = price_var,
      y = "Frecuencia"
    )

  ggsave(fs::path(out_dir, paste0("hist_", safe_name, "_p99.png")), g2, width = 10, height = 6)

  if (nrow(res_mes) > 0) {
    g3 <- ggplot(res_mes, aes(x = periodo_dt, y = mediana)) +
      geom_line() +
      labs(
        title = paste("Mediana mensual -", price_var),
        x = "Período",
        y = "Mediana mensual"
      )

    ggsave(fs::path(out_dir, paste0("serie_mediana_mensual_", safe_name, ".png")), g3, width = 11, height = 6)

    g4 <- ggplot(res_mes, aes(x = periodo_dt, y = media_pond_vol)) +
      geom_line() +
      labs(
        title = paste("Media mensual ponderada por volumen -", price_var),
        x = "Período",
        y = "Media ponderada"
      )

    ggsave(fs::path(out_dir, paste0("serie_media_pond_mensual_", safe_name, ".png")), g4, width = 11, height = 6)
  }

  # Export tables to Excel
  wb <- createWorkbook()

  addWorksheet(wb, "diag_crudo")
  writeData(wb, "diag_crudo", diag_crudo)

  addWorksheet(wb, "diag_parseo")
  writeData(wb, "diag_parseo", diag_parseo)

  addWorksheet(wb, "resumen_global")
  writeData(wb, "resumen_global", resumen_global)

  addWorksheet(wb, "quant_global")
  writeData(wb, "quant_global", quant_global)

  addWorksheet(wb, "res_anio")
  writeData(wb, "res_anio", res_anio)

  addWorksheet(wb, "res_producto")
  writeData(wb, "res_producto", res_producto)

  addWorksheet(wb, "res_anio_producto")
  writeData(wb, "res_anio_producto", res_anio_producto)

  addWorksheet(wb, "res_prod_canal")
  writeData(wb, "res_prod_canal", res_prod_canal)

  addWorksheet(wb, "top_max")
  writeData(wb, "top_max", top_max)

  addWorksheet(wb, "top_min_pos")
  writeData(wb, "top_min_pos", top_min_pos)

  addWorksheet(wb, "negativos")
  writeData(wb, "negativos", negativos)

  addWorksheet(wb, "out_global_res")
  writeData(wb, "out_global_res", out_global_res)

  addWorksheet(wb, "out_anio_res")
  writeData(wb, "out_anio_res", out_anio_res)

  addWorksheet(wb, "out_anio_prod_res")
  writeData(wb, "out_anio_prod_res", out_anio_prod_res)

  addWorksheet(wb, "outlier_rows")
  writeData(wb, "outlier_rows", outlier_rows_anio_prod)

  addWorksheet(wb, "outliers_estacion")
  writeData(wb, "outliers_estacion", outliers_estacion)

  if (nrow(res_mes) > 0) {
    addWorksheet(wb, "res_mes")
    writeData(wb, "res_mes", res_mes)
  }

  saveWorkbook(
    wb,
    fs::path(out_dir, paste0("analisis_", safe_name, ".xlsx")),
    overwrite = TRUE
  )

  invisible(list(
    diag_crudo = diag_crudo,
    diag_parseo = diag_parseo,
    resumen_global = resumen_global,
    quant_global = quant_global,
    res_anio = res_anio,
    res_producto = res_producto,
    res_anio_producto = res_anio_producto,
    res_prod_canal = res_prod_canal,
    top_max = top_max,
    top_min_pos = top_min_pos,
    negativos = negativos,
    outlier_rows = outlier_rows_anio_prod
  ))
}

## Run for every price variable ----

res_precios <- lapply(PRICE_VARS, function(v) analizar_precio(
  dt = eess_price,
  price_var = v,
  out_dir = DIR_OUT_PRICE
))

names(res_precios) <- PRICE_VARS

cat("\nDone. Outputs saved in:\n")
cat(DIR_OUT_PRICE, "\n")


# Price without taxes in detail ----

# Same diagnostics for precio_sin_impuestos with more detail: bands for
# near-zero prices, summaries by sales channel, ratio-to-median flags and
# outliers by year x product x sales channel.

library(data.table)
library(fs)
library(openxlsx)
library(ggplot2)

## Load the panel ----

DIR_DATASETS <- DIR_INTERIM
DIR_OUT_PRICE <- fs::path(DIR_DATASETS, "analisis_precio_sin_impuestos")
fs::dir_create(DIR_OUT_PRICE)

# Clear leftovers from the previous part or an earlier run
rm(list = c("base_clean", "eess_price", "res_precio_si"))
gc()

base_clean <- readRDS(fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds"))
setDT(base_clean)

eess_price <- copy(base_clean)
rm(base_clean)
gc()

cat("Panel loaded.\n")
cat("Rows:", nrow(eess_price), "\n")
cat("Columns:", ncol(eess_price), "\n")

## Auxiliary variables ----

if (!"anio" %in% names(eess_price)) {
  if ("periodo" %in% names(eess_price)) {
    eess_price[, anio := as.integer(substr(periodo, 1, 4))]
  } else if ("periodo_dt" %in% names(eess_price)) {
    eess_price[, anio := as.integer(format(periodo_dt, "%Y"))]
  } else {
    stop("Found neither 'anio' nor a period variable.")
  }
}

if (!"volumen_num" %in% names(eess_price) && "volumen" %in% names(eess_price)) {
  eess_price[, volumen_num := suppressWarnings(as.numeric(as.character(volumen)))]
}

precio_var <- "precio_sin_impuestos"
precio_chr <- trimws(as.character(eess_price[[precio_var]]))
eess_price[, precio_num := suppressWarnings(as.numeric(precio_chr))]

## Helpers ----

# Same helpers as in the previous part
q_safe <- function(x, p) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, probs = p, na.rm = TRUE))
}

min_safe <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

max_safe <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

## Raw diagnostics and parsing ----

diag_crudo <- data.table(
  variable = precio_var,
  n_total = nrow(eess_price),
  clase_original = paste(class(eess_price[[precio_var]]), collapse = " | "),
  n_na_original = sum(is.na(eess_price[[precio_var]])),
  n_blank = sum(!is.na(precio_chr) & precio_chr == ""),
  n_ND = sum(precio_chr == "ND", na.rm = TRUE),
  n_N_D = sum(precio_chr == "N/D", na.rm = TRUE),
  n_guion = sum(precio_chr == "-", na.rm = TRUE),
  n_con_coma = sum(grepl(",", precio_chr, fixed = TRUE), na.rm = TRUE),
  n_con_punto = sum(grepl(".", precio_chr, fixed = TRUE), na.rm = TRUE),
  n_con_espacios = sum(grepl(" ", precio_chr, fixed = TRUE), na.rm = TRUE)
)

parse_fail <- eess_price[
  !is.na(precio_chr) &
    precio_chr != "" &
    !(precio_chr %in% c("N/D", "ND", "-", "NA", "NULL")) &
    is.na(precio_num),
  .(valor = precio_chr)
]

diag_parseo <- data.table(
  variable = precio_var,
  n_total = nrow(eess_price),
  n_na_num = sum(is.na(eess_price$precio_num)),
  n_no_parseados_no_triviales = nrow(parse_fail),
  n_cero = sum(eess_price$precio_num == 0, na.rm = TRUE),
  n_negativos = sum(eess_price$precio_num < 0, na.rm = TRUE),
  n_positivos = sum(eess_price$precio_num > 0, na.rm = TRUE)
)

print(diag_crudo)
print(diag_parseo)

if (nrow(parse_fail) > 0) {
  cat("\nExamples of values that failed to parse:\n")
  print(head(unique(parse_fail$valor), 100))
}

cat("\nMost frequent raw values:\n")
print(head(sort(table(precio_chr), decreasing = TRUE), 50))

## Overall summary ----

resumen_global <- data.table(
  variable = precio_var,
  n = sum(!is.na(eess_price$precio_num)),
  media = mean(eess_price$precio_num, na.rm = TRUE),
  mediana = median(eess_price$precio_num, na.rm = TRUE),
  sd = sd(eess_price$precio_num, na.rm = TRUE),
  min = min_safe(eess_price$precio_num),
  max = max_safe(eess_price$precio_num)
)

quant_global <- data.table(
  prob = c(0, 0.0001, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 1),
  q = c(
    q_safe(eess_price$precio_num, 0),
    q_safe(eess_price$precio_num, 0.0001),
    q_safe(eess_price$precio_num, 0.001),
    q_safe(eess_price$precio_num, 0.005),
    q_safe(eess_price$precio_num, 0.01),
    q_safe(eess_price$precio_num, 0.05),
    q_safe(eess_price$precio_num, 0.10),
    q_safe(eess_price$precio_num, 0.25),
    q_safe(eess_price$precio_num, 0.50),
    q_safe(eess_price$precio_num, 0.75),
    q_safe(eess_price$precio_num, 0.90),
    q_safe(eess_price$precio_num, 0.95),
    q_safe(eess_price$precio_num, 0.99),
    q_safe(eess_price$precio_num, 0.999),
    q_safe(eess_price$precio_num, 1)
  )
)

print(resumen_global)
print(quant_global)

## Near-zero prices ----

eess_price[
  , banda_precio_chico := fifelse(
    is.na(precio_num), NA_character_,
    fifelse(precio_num == 0, "0",
            fifelse(precio_num <= 0.001, "(0,0.001]",
                    fifelse(precio_num <= 0.01, "(0.001,0.01]",
                            fifelse(precio_num <= 0.1, "(0.01,0.1]",
                                    fifelse(precio_num <= 1, "(0.1,1]",
                                            fifelse(precio_num <= 5, "(1,5]",
                                                    fifelse(precio_num <= 10, "(5,10]",
                                                            ">10"))))))))
]

precios_chicos_global <- eess_price[, .N, by = banda_precio_chico][order(banda_precio_chico)]
precios_chicos_anio <- eess_price[, .N, by = .(anio, banda_precio_chico)][order(anio, banda_precio_chico)]

print(precios_chicos_global)
print(precios_chicos_anio)

ejemplos_precios_chicos <- eess_price[
  !is.na(precio_num) & precio_num > 0 & precio_num <= 0.01,
  .(periodo, anio, nro_inscripcion, producto, canal_de_comercializacion, precio_sin_impuestos, precio_num, volumen_num, source_file)
][order(precio_num)]

print(ejemplos_precios_chicos[1:200])

## Summary by year ----

res_anio <- eess_price[
  , .(
    n = .N,
    n_na = sum(is.na(precio_num)),
    share_na = mean(is.na(precio_num)),
    n_cero = sum(precio_num == 0, na.rm = TRUE),
    share_cero = mean(precio_num == 0, na.rm = TRUE),
    n_neg = sum(precio_num < 0, na.rm = TRUE),
    media = mean(precio_num, na.rm = TRUE),
    mediana = median(precio_num, na.rm = TRUE),
    sd = sd(precio_num, na.rm = TRUE),
    p1 = q_safe(precio_num, 0.01),
    p5 = q_safe(precio_num, 0.05),
    p95 = q_safe(precio_num, 0.95),
    p99 = q_safe(precio_num, 0.99),
    p999 = q_safe(precio_num, 0.999),
    min_precio = min_safe(precio_num),
    max_precio = max_safe(precio_num)
  ),
  by = anio
][order(anio)]

print(res_anio)

## Summary by product ----

res_producto <- eess_price[
  , .(
    n = .N,
    n_na = sum(is.na(precio_num)),
    share_na = mean(is.na(precio_num)),
    n_cero = sum(precio_num == 0, na.rm = TRUE),
    share_cero = mean(precio_num == 0, na.rm = TRUE),
    n_neg = sum(precio_num < 0, na.rm = TRUE),
    media = mean(precio_num, na.rm = TRUE),
    mediana = median(precio_num, na.rm = TRUE),
    sd = sd(precio_num, na.rm = TRUE),
    p1 = q_safe(precio_num, 0.01),
    p5 = q_safe(precio_num, 0.05),
    p95 = q_safe(precio_num, 0.95),
    p99 = q_safe(precio_num, 0.99),
    p999 = q_safe(precio_num, 0.999),
    min_precio = min_safe(precio_num),
    max_precio = max_safe(precio_num)
  ),
  by = producto
][order(-n)]

print(res_producto)

## Summary by sales channel ----

res_canal <- eess_price[
  , .(
    n = .N,
    n_na = sum(is.na(precio_num)),
    share_na = mean(is.na(precio_num)),
    n_cero = sum(precio_num == 0, na.rm = TRUE),
    share_cero = mean(precio_num == 0, na.rm = TRUE),
    media = mean(precio_num, na.rm = TRUE),
    mediana = median(precio_num, na.rm = TRUE),
    p1 = q_safe(precio_num, 0.01),
    p5 = q_safe(precio_num, 0.05),
    p95 = q_safe(precio_num, 0.95),
    p99 = q_safe(precio_num, 0.99),
    min_precio = min_safe(precio_num),
    max_precio = max_safe(precio_num)
  ),
  by = canal_de_comercializacion
][order(-n)]

print(res_canal)

## Summary by product x sales channel ----

res_prod_canal <- eess_price[
  , .(
    n = .N,
    n_na = sum(is.na(precio_num)),
    share_na = mean(is.na(precio_num)),
    n_cero = sum(precio_num == 0, na.rm = TRUE),
    share_cero = mean(precio_num == 0, na.rm = TRUE),
    mediana = median(precio_num, na.rm = TRUE),
    p5 = q_safe(precio_num, 0.05),
    p95 = q_safe(precio_num, 0.95),
    p99 = q_safe(precio_num, 0.99),
    min_precio = min_safe(precio_num),
    max_precio = max_safe(precio_num)
  ),
  by = .(producto, canal_de_comercializacion)
][order(producto, -n)]

print(res_prod_canal)

## Summary by year x product ----

res_anio_producto <- eess_price[
  , .(
    n = .N,
    n_na = sum(is.na(precio_num)),
    share_na = mean(is.na(precio_num)),
    n_cero = sum(precio_num == 0, na.rm = TRUE),
    share_cero = mean(precio_num == 0, na.rm = TRUE),
    media = mean(precio_num, na.rm = TRUE),
    mediana = median(precio_num, na.rm = TRUE),
    p1 = q_safe(precio_num, 0.01),
    p5 = q_safe(precio_num, 0.05),
    p95 = q_safe(precio_num, 0.95),
    p99 = q_safe(precio_num, 0.99),
    p999 = q_safe(precio_num, 0.999),
    min_precio = min_safe(precio_num),
    max_precio = max_safe(precio_num)
  ),
  by = .(anio, producto)
][order(anio, producto)]

print(res_anio_producto)

## Highest and lowest prices ----

top_max <- eess_price[
  !is.na(precio_num)
][order(-precio_num),
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio,
    canal_de_comercializacion, precio_sin_impuestos, precio_num, volumen_num, source_file)
][1:300]

top_min_pos <- eess_price[
  !is.na(precio_num) & precio_num > 0
][order(precio_num),
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio,
    canal_de_comercializacion, precio_sin_impuestos, precio_num, volumen_num, source_file)
][1:300]

negativos <- eess_price[
  !is.na(precio_num) & precio_num < 0,
  .(periodo, anio, nro_inscripcion, producto, tipo_negocio,
    canal_de_comercializacion, precio_sin_impuestos, precio_num, volumen_num, source_file)
][order(precio_num)]

print(top_max[1:50])
print(top_min_pos[1:50])
print(negativos)

## Outliers against the pooled distribution ----

# Flags: 1.5 x IQR fences, outside p1-p99, above p99.9, and above 100 times the
# pooled median
q25_g <- q_safe(eess_price$precio_num, 0.25)
q75_g <- q_safe(eess_price$precio_num, 0.75)
iqr_g <- q75_g - q25_g
p1_g <- q_safe(eess_price$precio_num, 0.01)
p99_g <- q_safe(eess_price$precio_num, 0.99)
p999_g <- q_safe(eess_price$precio_num, 0.999)
med_g <- median(eess_price$precio_num, na.rm = TRUE)

eess_price[, flag_out_global_iqr :=
             precio_num < (q25_g - 1.5 * iqr_g) |
             precio_num > (q75_g + 1.5 * iqr_g)]

eess_price[, flag_out_global_p99 :=
             precio_num < p1_g | precio_num > p99_g]

eess_price[, flag_out_global_p999 :=
             precio_num > p999_g]

eess_price[, flag_out_global_ratio :=
             !is.na(precio_num) & med_g > 0 & precio_num > 100 * med_g]

out_global_res <- eess_price[, .(
  n_total = .N,
  n_out_iqr = sum(flag_out_global_iqr, na.rm = TRUE),
  n_out_p99 = sum(flag_out_global_p99, na.rm = TRUE),
  n_out_p999 = sum(flag_out_global_p999, na.rm = TRUE),
  n_out_ratio = sum(flag_out_global_ratio, na.rm = TRUE)
)]

print(out_global_res)

## Outliers within year ----

# Same flags with year-specific cutoffs; the ratio flag uses 50 times the median
cut_anio <- eess_price[
  !is.na(precio_num),
  .(
    n = .N,
    q25 = q_safe(precio_num, 0.25),
    q75 = q_safe(precio_num, 0.75),
    p1 = q_safe(precio_num, 0.01),
    p99 = q_safe(precio_num, 0.99),
    p999 = q_safe(precio_num, 0.999),
    mediana = median(precio_num, na.rm = TRUE)
  ),
  by = anio
]

cut_anio[, iqr := q75 - q25]

eess_price <- cut_anio[eess_price, on = "anio"]

eess_price[, flag_out_anio_iqr :=
             precio_num < (q25 - 1.5 * iqr) |
             precio_num > (q75 + 1.5 * iqr)]

eess_price[, flag_out_anio_p99 :=
             precio_num < p1 | precio_num > p99]

eess_price[, flag_out_anio_p999 :=
             precio_num > p999]

eess_price[, flag_out_anio_ratio :=
             mediana > 0 & precio_num > 50 * mediana]

out_anio_res <- eess_price[
  , .(
    n = .N,
    n_out_iqr = sum(flag_out_anio_iqr, na.rm = TRUE),
    n_out_p99 = sum(flag_out_anio_p99, na.rm = TRUE),
    n_out_p999 = sum(flag_out_anio_p999, na.rm = TRUE),
    n_out_ratio = sum(flag_out_anio_ratio, na.rm = TRUE)
  ),
  by = anio
][order(anio)]

print(out_anio_res)

## Outliers within year x product ----

# The grouping that matters most, since price levels differ across products and
# years. Cells with fewer than 20 observations are not flagged; the ratio flag
# uses 20 times the cell median.
cut_anio_prod <- eess_price[
  !is.na(precio_num),
  .(
    n_gp = .N,
    q25_gp = q_safe(precio_num, 0.25),
    q75_gp = q_safe(precio_num, 0.75),
    p1_gp = q_safe(precio_num, 0.01),
    p99_gp = q_safe(precio_num, 0.99),
    p999_gp = q_safe(precio_num, 0.999),
    mediana_gp = median(precio_num, na.rm = TRUE)
  ),
  by = .(anio, producto)
]

cut_anio_prod[, iqr_gp := q75_gp - q25_gp]

eess_price <- cut_anio_prod[eess_price, on = .(anio, producto)]

eess_price[, flag_out_anio_prod_iqr :=
             n_gp >= 20 &
             (precio_num < (q25_gp - 1.5 * iqr_gp) |
                precio_num > (q75_gp + 1.5 * iqr_gp))]

eess_price[, flag_out_anio_prod_p99 :=
             n_gp >= 20 &
             (precio_num < p1_gp | precio_num > p99_gp)]

eess_price[, flag_out_anio_prod_p999 :=
             n_gp >= 20 & precio_num > p999_gp]

eess_price[, flag_out_anio_prod_ratio :=
             n_gp >= 20 & mediana_gp > 0 & precio_num > 20 * mediana_gp]

out_anio_prod_res <- eess_price[
  , .(
    n = .N,
    n_out_iqr = sum(flag_out_anio_prod_iqr, na.rm = TRUE),
    n_out_p99 = sum(flag_out_anio_prod_p99, na.rm = TRUE),
    n_out_p999 = sum(flag_out_anio_prod_p999, na.rm = TRUE),
    n_out_ratio = sum(flag_out_anio_prod_ratio, na.rm = TRUE)
  ),
  by = .(anio, producto)
][order(anio, producto)]

print(out_anio_prod_res)

## Outliers within year x product x sales channel ----

cut_anio_prod_canal <- eess_price[
  !is.na(precio_num),
  .(
    n_gpc = .N,
    q25_gpc = q_safe(precio_num, 0.25),
    q75_gpc = q_safe(precio_num, 0.75),
    p1_gpc = q_safe(precio_num, 0.01),
    p99_gpc = q_safe(precio_num, 0.99),
    mediana_gpc = median(precio_num, na.rm = TRUE)
  ),
  by = .(anio, producto, canal_de_comercializacion)
]

cut_anio_prod_canal[, iqr_gpc := q75_gpc - q25_gpc]

eess_price <- cut_anio_prod_canal[eess_price, on = .(anio, producto, canal_de_comercializacion)]

eess_price[, flag_out_anio_prod_canal_iqr :=
             n_gpc >= 20 &
             (precio_num < (q25_gpc - 1.5 * iqr_gpc) |
                precio_num > (q75_gpc + 1.5 * iqr_gpc))]

eess_price[, flag_out_anio_prod_canal_p99 :=
             n_gpc >= 20 &
             (precio_num < p1_gpc | precio_num > p99_gpc)]

out_anio_prod_canal_res <- eess_price[
  , .(
    n = .N,
    n_out_iqr = sum(flag_out_anio_prod_canal_iqr, na.rm = TRUE),
    n_out_p99 = sum(flag_out_anio_prod_canal_p99, na.rm = TRUE)
  ),
  by = .(anio, producto, canal_de_comercializacion)
][order(anio, producto, canal_de_comercializacion)]

print(out_anio_prod_canal_res)

## Flagged rows ----

outlier_rows <- eess_price[
  flag_out_anio_prod_p999 == TRUE |
    flag_out_anio_prod_iqr == TRUE |
    flag_out_anio_prod_ratio == TRUE,
  .(
    periodo, anio, nro_inscripcion, producto, tipo_negocio, canal_de_comercializacion,
    precio_sin_impuestos, precio_num, volumen_num,
    mediana_gp, p99_gp, p999_gp, q25_gp, q75_gp,
    source_file
  )
][order(anio, producto, -precio_num)]

print(outlier_rows[1:500])

## Stations that concentrate the outliers ----

outliers_estacion <- eess_price[
  flag_out_anio_prod_p999 == TRUE |
    flag_out_anio_prod_iqr == TRUE |
    flag_out_anio_prod_ratio == TRUE,
  .(
    n_outliers = .N,
    productos = paste(sort(unique(producto)), collapse = " | "),
    canales = paste(sort(unique(canal_de_comercializacion)), collapse = " | "),
    min_anio = min(anio, na.rm = TRUE),
    max_anio = max(anio, na.rm = TRUE),
    max_precio = max(precio_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_outliers, -max_precio)]

print(outliers_estacion[1:200])

## Monthly series ----

if ("periodo_dt" %in% names(eess_price)) {
  res_mes <- eess_price[
    , .(
      n = .N,
      n_na = sum(is.na(precio_num)),
      n_cero = sum(precio_num == 0, na.rm = TRUE),
      media = mean(precio_num, na.rm = TRUE),
      mediana = median(precio_num, na.rm = TRUE),
      p5 = q_safe(precio_num, 0.05),
      p95 = q_safe(precio_num, 0.95)
    ),
    by = periodo_dt
  ][order(periodo_dt)]

  res_mes_producto <- eess_price[
    , .(
      n = .N,
      media = mean(precio_num, na.rm = TRUE),
      mediana = median(precio_num, na.rm = TRUE),
      p5 = q_safe(precio_num, 0.05),
      p95 = q_safe(precio_num, 0.95)
    ),
    by = .(periodo_dt, producto)
  ][order(periodo_dt, producto)]
} else {
  res_mes <- data.table()
  res_mes_producto <- data.table()
}

print(head(res_mes, 24))
print(tail(res_mes, 24))

## Figures ----

p99_global <- q_safe(eess_price$precio_num, 0.99)

g1 <- ggplot(
  eess_price[!is.na(precio_num) & is.finite(precio_num)],
  aes(x = precio_num)
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución completa de precio_sin_impuestos",
    x = "precio_sin_impuestos",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_PRICE, "hist_precio_sin_impuestos_completo.png"), g1, width = 10, height = 6)

g2 <- ggplot(
  eess_price[!is.na(precio_num) & precio_num <= p99_global],
  aes(x = precio_num)
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de precio_sin_impuestos truncada al p99 global",
    x = "precio_sin_impuestos",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_PRICE, "hist_precio_sin_impuestos_p99.png"), g2, width = 10, height = 6)

g3 <- ggplot(
  eess_price[!is.na(precio_num) & precio_num > 0],
  aes(x = log(precio_num))
) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de log(precio_sin_impuestos)",
    x = "log(precio_sin_impuestos)",
    y = "Frecuencia"
  )

ggsave(fs::path(DIR_OUT_PRICE, "hist_log_precio_sin_impuestos.png"), g3, width = 10, height = 6)

if (nrow(res_mes) > 0) {
  g4 <- ggplot(res_mes, aes(x = periodo_dt, y = mediana)) +
    geom_line() +
    labs(
      title = "Mediana mensual de precio_sin_impuestos",
      x = "Período",
      y = "Mediana mensual"
    )

  ggsave(fs::path(DIR_OUT_PRICE, "serie_mediana_mensual_precio_sin_impuestos.png"), g4, width = 11, height = 6)

  g5 <- ggplot(res_mes, aes(x = periodo_dt, y = p95)) +
    geom_line() +
    labs(
      title = "P95 mensual de precio_sin_impuestos",
      x = "Período",
      y = "P95 mensual"
    )

  ggsave(fs::path(DIR_OUT_PRICE, "serie_p95_mensual_precio_sin_impuestos.png"), g5, width = 11, height = 6)
}

## Export tables to Excel ----

wb <- createWorkbook()

addWorksheet(wb, "diag_crudo")
writeData(wb, "diag_crudo", diag_crudo)

addWorksheet(wb, "diag_parseo")
writeData(wb, "diag_parseo", diag_parseo)

addWorksheet(wb, "resumen_global")
writeData(wb, "resumen_global", resumen_global)

addWorksheet(wb, "quant_global")
writeData(wb, "quant_global", quant_global)

addWorksheet(wb, "precios_chicos_global")
writeData(wb, "precios_chicos_global", precios_chicos_global)

addWorksheet(wb, "precios_chicos_anio")
writeData(wb, "precios_chicos_anio", precios_chicos_anio)

addWorksheet(wb, "ej_precios_chicos")
writeData(wb, "ej_precios_chicos", ejemplos_precios_chicos)

addWorksheet(wb, "res_anio")
writeData(wb, "res_anio", res_anio)

addWorksheet(wb, "res_producto")
writeData(wb, "res_producto", res_producto)

addWorksheet(wb, "res_canal")
writeData(wb, "res_canal", res_canal)

addWorksheet(wb, "res_prod_canal")
writeData(wb, "res_prod_canal", res_prod_canal)

addWorksheet(wb, "res_anio_producto")
writeData(wb, "res_anio_producto", res_anio_producto)

addWorksheet(wb, "top_max")
writeData(wb, "top_max", top_max)

addWorksheet(wb, "top_min_pos")
writeData(wb, "top_min_pos", top_min_pos)

addWorksheet(wb, "negativos")
writeData(wb, "negativos", negativos)

addWorksheet(wb, "out_global_res")
writeData(wb, "out_global_res", out_global_res)

addWorksheet(wb, "out_anio_res")
writeData(wb, "out_anio_res", out_anio_res)

addWorksheet(wb, "out_anio_prod_res")
writeData(wb, "out_anio_prod_res", out_anio_prod_res)

addWorksheet(wb, "out_anio_prod_canal")
writeData(wb, "out_anio_prod_canal", out_anio_prod_canal_res)

addWorksheet(wb, "outlier_rows")
writeData(wb, "outlier_rows", outlier_rows)

addWorksheet(wb, "outliers_estacion")
writeData(wb, "outliers_estacion", outliers_estacion)

if (nrow(res_mes) > 0) {
  addWorksheet(wb, "res_mes")
  writeData(wb, "res_mes", res_mes)
}

if (nrow(res_mes_producto) > 0) {
  addWorksheet(wb, "res_mes_producto")
  writeData(wb, "res_mes_producto", res_mes_producto)
}

saveWorkbook(
  wb,
  fs::path(DIR_OUT_PRICE, "analisis_exhaustivo_precio_sin_impuestos.xlsx"),
  overwrite = TRUE
)

cat("\nDone. Outputs saved in:\n")
cat(DIR_OUT_PRICE, "\n")

# 03_clean_prices.R
# Flags extreme values of precio_sin_impuestos (price net of taxes) and drops
# them: zeros, positive prices of 0.1 or less and prices above 100,000.
#
# Input:  eess_all_cleaned3_cut.rds
# Output: eess_all_cleaned4_cut.rds

library(data.table)
library(fs)

source("code/00_config.R")

DIR_DATASETS <- DIR_INTERIM
FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds")

if (!fs::file_exists(FILE_BASE)) {
  stop("File not found: ", FILE_BASE)
}

eess_all_cleaned3_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned3_cut)

cat("Data loaded\n")
cat("Rows:", nrow(eess_all_cleaned3_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned3_cut), "\n")

# 1. Basic check of the price variable ----

var_precio <- "precio_sin_impuestos"

if (!var_precio %in% names(eess_all_cleaned3_cut)) {
  stop("Variable ", var_precio, " is not in the data.")
}

cat("\nOriginal class of the variable:\n")
print(class(eess_all_cleaned3_cut[[var_precio]]))

cat("\nFirst values of the original variable:\n")
print(head(eess_all_cleaned3_cut[[var_precio]], 20))

# 2. Numeric version ----

# The original column is left untouched: a trimmed text copy and a numeric
# version are added as new columns.
eess_all_cleaned3_cut[, precio_sin_impuestos_chr := trimws(as.character(get(var_precio)))]

eess_all_cleaned3_cut[, precio_sin_impuestos_num := suppressWarnings(
  as.numeric(precio_sin_impuestos_chr)
)]

# 3. Conversion diagnostics ----

# Values that fail to parse and are not one of the usual missing-value codes
parse_fail <- eess_all_cleaned3_cut[
  !is.na(precio_sin_impuestos_chr) &
    precio_sin_impuestos_chr != "" &
    !(precio_sin_impuestos_chr %in% c("N/D", "ND", "-", "NA", "NULL")) &
    is.na(precio_sin_impuestos_num),
  .(valor = precio_sin_impuestos_chr)
]

diag_precio <- data.table(
  variable = var_precio,
  n_total = nrow(eess_all_cleaned3_cut),
  clase_original = paste(class(eess_all_cleaned3_cut[[var_precio]]), collapse = " | "),
  n_na_original = sum(is.na(eess_all_cleaned3_cut[[var_precio]])),
  n_blank = sum(!is.na(eess_all_cleaned3_cut$precio_sin_impuestos_chr) &
                  eess_all_cleaned3_cut$precio_sin_impuestos_chr == ""),
  n_con_coma = sum(grepl(",", eess_all_cleaned3_cut$precio_sin_impuestos_chr, fixed = TRUE), na.rm = TRUE),
  n_con_punto = sum(grepl(".", eess_all_cleaned3_cut$precio_sin_impuestos_chr, fixed = TRUE), na.rm = TRUE),
  n_con_espacios = sum(grepl(" ", eess_all_cleaned3_cut$precio_sin_impuestos_chr, fixed = TRUE), na.rm = TRUE),
  n_na_num = sum(is.na(eess_all_cleaned3_cut$precio_sin_impuestos_num)),
  n_no_parseados_no_triviales = nrow(parse_fail),
  n_cero = sum(eess_all_cleaned3_cut$precio_sin_impuestos_num == 0, na.rm = TRUE),
  n_negativos = sum(eess_all_cleaned3_cut$precio_sin_impuestos_num < 0, na.rm = TRUE),
  n_positivos = sum(eess_all_cleaned3_cut$precio_sin_impuestos_num > 0, na.rm = TRUE)
)

cat("\nConversion diagnostics:\n")
print(diag_precio)

if (nrow(parse_fail) > 0) {
  cat("\nExamples of values that could not be parsed as numeric:\n")
  print(head(unique(parse_fail$valor), 100))
} else {
  cat("\nNo parsing problems.\n")
}

# 4. Distribution of the numeric price ----

cat("\nClass of the numeric version:\n")
print(class(eess_all_cleaned3_cut$precio_sin_impuestos_num))

cat("\nSummary of the numeric version:\n")
print(summary(eess_all_cleaned3_cut$precio_sin_impuestos_num))

quant_precio <- data.table(
  prob = c(0, 0.0001, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 1),
  q = as.numeric(quantile(
    eess_all_cleaned3_cut$precio_sin_impuestos_num,
    probs = c(0, 0.0001, 0.001, 0.005, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 1),
    na.rm = TRUE
  ))
)

cat("\nQuantiles of the numeric version:\n")
print(quant_precio)

cat("\nMost frequent values of the original variable:\n")
print(head(sort(table(eess_all_cleaned3_cut$precio_sin_impuestos_chr), decreasing = TRUE), 50))

cat("\nSmallest positive values of the numeric version:\n")
print(
  eess_all_cleaned3_cut[
    !is.na(precio_sin_impuestos_num) & precio_sin_impuestos_num > 0,
    .(precio_sin_impuestos, precio_sin_impuestos_chr, precio_sin_impuestos_num)
  ][order(precio_sin_impuestos_num)][1:50]
)

cat("\nLargest values of the numeric version:\n")
print(
  eess_all_cleaned3_cut[
    !is.na(precio_sin_impuestos_num),
    .(periodo, anio, nro_inscripcion, producto, canal_de_comercializacion,
      precio_sin_impuestos, precio_sin_impuestos_num, volumen)
  ][order(-precio_sin_impuestos_num)][1:50]
)

# 5. Summary by year ----

# anio is rebuilt from periodo if the column is not there
if (!"anio" %in% names(eess_all_cleaned3_cut)) {
  if ("periodo" %in% names(eess_all_cleaned3_cut)) {
    eess_all_cleaned3_cut[, anio := as.integer(substr(periodo, 1, 4))]
  }
}

res_anio_precio <- eess_all_cleaned3_cut[
  , .(
    n = .N,
    n_na = sum(is.na(precio_sin_impuestos_num)),
    n_cero = sum(precio_sin_impuestos_num == 0, na.rm = TRUE),
    n_neg = sum(precio_sin_impuestos_num < 0, na.rm = TRUE),
    media = mean(precio_sin_impuestos_num, na.rm = TRUE),
    mediana = median(precio_sin_impuestos_num, na.rm = TRUE),
    p1 = as.numeric(quantile(precio_sin_impuestos_num, 0.01, na.rm = TRUE)),
    p5 = as.numeric(quantile(precio_sin_impuestos_num, 0.05, na.rm = TRUE)),
    p95 = as.numeric(quantile(precio_sin_impuestos_num, 0.95, na.rm = TRUE)),
    p99 = as.numeric(quantile(precio_sin_impuestos_num, 0.99, na.rm = TRUE)),
    min_precio = min(precio_sin_impuestos_num, na.rm = TRUE),
    max_precio = max(precio_sin_impuestos_num, na.rm = TRUE)
  ),
  by = anio
][order(anio)]

cat("\nSummary by year:\n")
print(res_anio_precio)

# 6. Flags for extreme prices ----

if (!"precio_sin_impuestos_num" %in% names(eess_all_cleaned3_cut)) {
  stop("precio_sin_impuestos_num does not exist. Run the conversion block first.")
}

# Conservative flags: exact zeros and positive prices of 0.01 or less
eess_all_cleaned3_cut[, flag_precio_cero :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num == 0]

eess_all_cleaned3_cut[, flag_precio_muy_bajo :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num > 0 &
                        precio_sin_impuestos_num <= 0.01]

# Prices in (0.01, 0.1] are suspicious; flagged separately so they can be
# inspected before deciding whether to drop them.
eess_all_cleaned3_cut[, flag_precio_bajo_sospechoso :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num > 0.01 &
                        precio_sin_impuestos_num <= 0.1]

# High prices, by order of magnitude
eess_all_cleaned3_cut[, flag_precio_alto_100k :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num > 100000]

eess_all_cleaned3_cut[, flag_precio_alto_1m :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num > 1000000]

eess_all_cleaned3_cut[, flag_precio_alto_10m :=
                        !is.na(precio_sin_impuestos_num) &
                        precio_sin_impuestos_num > 10000000]

# First, cautious removal rule (v1): zero, (0, 0.01] or above 100,000
eess_all_cleaned3_cut[, flag_precio_remove_v1 :=
                        flag_precio_cero |
                        flag_precio_muy_bajo |
                        flag_precio_alto_100k]

# 7. Flag counts: overall, by year and by product ----

res_flags_global <- eess_all_cleaned3_cut[, .(
  n_total = .N,
  n_precio_cero = sum(flag_precio_cero, na.rm = TRUE),
  n_precio_muy_bajo = sum(flag_precio_muy_bajo, na.rm = TRUE),
  n_precio_bajo_sospechoso = sum(flag_precio_bajo_sospechoso, na.rm = TRUE),
  n_precio_alto_100k = sum(flag_precio_alto_100k, na.rm = TRUE),
  n_precio_alto_1m = sum(flag_precio_alto_1m, na.rm = TRUE),
  n_precio_alto_10m = sum(flag_precio_alto_10m, na.rm = TRUE),
  n_remove_v1 = sum(flag_precio_remove_v1, na.rm = TRUE)
)]

cat("\nOverall flag summary:\n")
print(res_flags_global)

res_flags_anio <- eess_all_cleaned3_cut[, .(
  n = .N,
  n_precio_cero = sum(flag_precio_cero, na.rm = TRUE),
  n_precio_muy_bajo = sum(flag_precio_muy_bajo, na.rm = TRUE),
  n_precio_bajo_sospechoso = sum(flag_precio_bajo_sospechoso, na.rm = TRUE),
  n_precio_alto_100k = sum(flag_precio_alto_100k, na.rm = TRUE),
  n_precio_alto_1m = sum(flag_precio_alto_1m, na.rm = TRUE),
  n_precio_alto_10m = sum(flag_precio_alto_10m, na.rm = TRUE),
  n_remove_v1 = sum(flag_precio_remove_v1, na.rm = TRUE)
), by = anio][order(anio)]

cat("\nSummary by year:\n")
print(res_flags_anio)

res_flags_producto <- eess_all_cleaned3_cut[, .(
  n = .N,
  n_precio_cero = sum(flag_precio_cero, na.rm = TRUE),
  n_precio_muy_bajo = sum(flag_precio_muy_bajo, na.rm = TRUE),
  n_precio_bajo_sospechoso = sum(flag_precio_bajo_sospechoso, na.rm = TRUE),
  n_precio_alto_100k = sum(flag_precio_alto_100k, na.rm = TRUE),
  n_precio_alto_1m = sum(flag_precio_alto_1m, na.rm = TRUE),
  n_precio_alto_10m = sum(flag_precio_alto_10m, na.rm = TRUE),
  n_remove_v1 = sum(flag_precio_remove_v1, na.rm = TRUE)
), by = producto][order(-n_remove_v1, -n)]

cat("\nSummary by product:\n")
print(res_flags_producto)

# 8. Examples of flagged records ----

ej_bajos <- eess_all_cleaned3_cut[
  flag_precio_cero | flag_precio_muy_bajo | flag_precio_bajo_sospechoso,
  .(
    periodo, anio, provincia, nro_inscripcion, producto,
    canal_de_comercializacion, precio_sin_impuestos,
    precio_sin_impuestos_num, volumen
  )
][order(precio_sin_impuestos_num)]

cat("\nFirst examples of low/extreme prices:\n")
print(ej_bajos[1:100])

ej_altos_100k <- eess_all_cleaned3_cut[
  flag_precio_alto_100k == TRUE,
  .(
    periodo, anio, provincia, nro_inscripcion, producto,
    canal_de_comercializacion, precio_sin_impuestos,
    precio_sin_impuestos_num, volumen
  )
][order(-precio_sin_impuestos_num)]

cat("\nFirst examples of prices > 100000:\n")
print(ej_altos_100k[1:100])

# 9. Frequency table by price band ----

eess_all_cleaned3_cut[, banda_precio_extremos := fifelse(
  is.na(precio_sin_impuestos_num), NA_character_,
  fifelse(precio_sin_impuestos_num == 0, "0",
          fifelse(precio_sin_impuestos_num <= 0.01, "(0,0.01]",
                  fifelse(precio_sin_impuestos_num <= 0.1, "(0.01,0.1]",
                          fifelse(precio_sin_impuestos_num <= 1, "(0.1,1]",
                                  fifelse(precio_sin_impuestos_num <= 1000, "(1,1000]",
                                          fifelse(precio_sin_impuestos_num <= 10000, "(1000,10000]",
                                                  fifelse(precio_sin_impuestos_num <= 100000, "(10000,100000]",
                                                          fifelse(precio_sin_impuestos_num <= 1000000, "(100000,1000000]",
                                                                  ">1000000")))))))))
]

tabla_bandas <- eess_all_cleaned3_cut[, .N, by = banda_precio_extremos][order(
  factor(
    banda_precio_extremos,
    levels = c("0", "(0,0.01]", "(0.01,0.1]", "(0.1,1]", "(1,1000]",
               "(1000,10000]", "(10000,100000]", "(100000,1000000]", ">1000000")
  )
)]

cat("\nTable by price band:\n")
print(tabla_bandas)

# 10. Tentative clean variable (v1) ----

# In memory only; nothing is saved at this step
eess_all_cleaned3_cut[, precio_sin_impuestos_clean_v1 := precio_sin_impuestos_num]
eess_all_cleaned3_cut[flag_precio_remove_v1 == TRUE, precio_sin_impuestos_clean_v1 := NA_real_]

comparacion_clean_v1 <- eess_all_cleaned3_cut[, .(
  n_original_no_na = sum(!is.na(precio_sin_impuestos_num)),
  n_clean_v1_no_na = sum(!is.na(precio_sin_impuestos_clean_v1)),
  media_original = mean(precio_sin_impuestos_num, na.rm = TRUE),
  media_clean_v1 = mean(precio_sin_impuestos_clean_v1, na.rm = TRUE),
  mediana_original = median(precio_sin_impuestos_num, na.rm = TRUE),
  mediana_clean_v1 = median(precio_sin_impuestos_clean_v1, na.rm = TRUE),
  max_original = max(precio_sin_impuestos_num, na.rm = TRUE),
  max_clean_v1 = max(precio_sin_impuestos_clean_v1, na.rm = TRUE)
)]

cat("\nOriginal vs clean_v1:\n")
print(comparacion_clean_v1)

# 11. Final removal flag (v2) and filtered panel ----

# v2 adds the (0.01, 0.1] range to the v1 rule
eess_all_cleaned3_cut[, flag_precio_remove_v2 :=
                        flag_precio_cero |
                        flag_precio_muy_bajo |
                        flag_precio_bajo_sospechoso |
                        flag_precio_alto_100k]

res_flags_final <- eess_all_cleaned3_cut[, .(
  n_total = .N,
  n_remove_v2 = sum(flag_precio_remove_v2, na.rm = TRUE),
  n_keep_v2 = sum(!flag_precio_remove_v2, na.rm = TRUE)
)]

cat("\nFinal removal summary (v2):\n")
print(res_flags_final)

# The auxiliary columns were appended at the end, so the first 29 columns are
# the original ones.
vars_originales <- names(eess_all_cleaned3_cut)[1:29]

cat("\nOriginal variables kept:\n")
print(vars_originales)

# New panel: original variables only, without the extreme price observations.
# Rows with a missing price are kept.
eess_all_cleaned3_cut_preciofiltrado <- copy(
  eess_all_cleaned3_cut[flag_precio_remove_v2 == FALSE, ..vars_originales]
)

cat("\nNew data set created in memory: eess_all_cleaned3_cut_preciofiltrado\n")
cat("Rows:", nrow(eess_all_cleaned3_cut_preciofiltrado), "\n")
cat("Columns:", ncol(eess_all_cleaned3_cut_preciofiltrado), "\n")

# Rebuild the numeric price only to check the result
eess_all_cleaned3_cut_preciofiltrado[
  , precio_sin_impuestos_num_check := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))
]

check_final_precio <- eess_all_cleaned3_cut_preciofiltrado[, .(
  n = .N,
  n_na = sum(is.na(precio_sin_impuestos_num_check)),
  n_cero = sum(precio_sin_impuestos_num_check == 0, na.rm = TRUE),
  n_leq_001 = sum(precio_sin_impuestos_num_check > 0 & precio_sin_impuestos_num_check <= 0.01, na.rm = TRUE),
  n_001_01 = sum(precio_sin_impuestos_num_check > 0.01 & precio_sin_impuestos_num_check <= 0.1, na.rm = TRUE),
  n_gt_100k = sum(precio_sin_impuestos_num_check > 100000, na.rm = TRUE),
  mediana = median(precio_sin_impuestos_num_check, na.rm = TRUE),
  media = mean(precio_sin_impuestos_num_check, na.rm = TRUE),
  max_precio = max(precio_sin_impuestos_num_check, na.rm = TRUE)
)]

cat("\nFinal price check on the new data set:\n")
print(check_final_precio)

eess_all_cleaned3_cut_preciofiltrado[, precio_sin_impuestos_num_check := NULL]

# 12. Save ----

DIR_DATASETS <- DIR_INTERIM
FILE_OUT <- fs::path(DIR_DATASETS, "eess_all_cleaned4_cut.rds")

saveRDS(eess_all_cleaned3_cut_preciofiltrado, FILE_OUT)

cat("Data saved to:\n")
cat(FILE_OUT, "\n")

# Read the file back and check its dimensions
tmp_check <- readRDS(FILE_OUT)
setDT(tmp_check)

cat("\nCheck of the saved file:\n")
cat("Rows:", nrow(tmp_check), "\n")
cat("Columns:", ncol(tmp_check), "\n")

rm(tmp_check)
gc()

# Same panel under its cleaned4 name, written again to the same file
eess_all_cleaned4_cut <- copy(eess_all_cleaned3_cut_preciofiltrado)
saveRDS(eess_all_cleaned4_cut, FILE_OUT)

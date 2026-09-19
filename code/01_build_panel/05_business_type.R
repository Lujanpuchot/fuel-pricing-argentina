# 05_business_type.R
# Replaces the generic label "Estación de servicio" in tipo_negocio (business
# type) with a type inferred from the products each boca (outlet) sells.
#
# Input:  eess_all_cleaned5_cut.rds
# Output: eess_all_cleaned6_cut_nostations.rds (type inferred month by month)
#         eess_all_cleaned7_alternative_sinceappearance.rds (final panel)

library(data.table)
library(fs)

source("code/00_config.R")

DIR_DATASETS <- DIR_INTERIM
FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")

# Parts 1 to 4 diagnose tipo_negocio, infer the type month by month, save that
# panel and check how stable the monthly type is within a station. Parts 5 and 6
# build station-level alternatives (modal type, "ever", "since first
# appearance"), each starting again from the panel on disk. Part 7 saves the
# final panel, which uses the "since first appearance" version.
# A boca-month is one outlet (nro_inscripcion) in one month (periodo_dt).


# 1. Diagnosis of tipo_negocio ----

# The source uses the generic label "Estación de servicio" alongside the
# detailed "Bocas de expendio ..." categories until July 2022 and drops it
# afterwards. This part lists the labels, gives the first and last period of
# each and counts the stations that carry both.

if (!fs::file_exists(FILE_BASE)) {
  stop("File not found: ", FILE_BASE)
}

eess_all_cleaned5_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned5_cut)

cat("Panel loaded\n")
cat("Rows:", nrow(eess_all_cleaned5_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned5_cut), "\n")

req_vars <- c("periodo_dt", "nro_inscripcion", "tipo_negocio", "producto")
stopifnot(all(req_vars %in% names(eess_all_cleaned5_cut)))

# periodo_dt is expected to be a date already
if (!inherits(eess_all_cleaned5_cut$periodo_dt, c("IDate", "Date"))) {
  eess_all_cleaned5_cut[, periodo_dt := as.IDate(periodo_dt)]
}

# Labels are compared in lower case, without accents or surrounding blanks
norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

dt <- copy(eess_all_cleaned5_cut)

dt[, tipo_negocio_raw  := as.character(tipo_negocio)]
dt[, tipo_negocio_norm := norm_txt(tipo_negocio)]
dt[, producto_norm     := norm_txt(producto)]

if ("canal_de_comercializacion" %in% names(dt)) {
  dt[, canal_norm := norm_txt(canal_de_comercializacion)]
}

# Labels that mention "estacion" or "boca"
labels_tipo_relevantes <- sort(unique(
  dt[grepl("estacion|boca|bocas", tipo_negocio_norm), tipo_negocio_raw]
))

cat("\ntipo_negocio labels that mention estacion/boca:\n")
print(labels_tipo_relevantes)

# First and last period of each exact label
tipo_periodo <- dt[
  grepl("estacion|boca|bocas", tipo_negocio_norm) & !is.na(periodo_dt),
  .(
    min_periodo = min(periodo_dt, na.rm = TRUE),
    max_periodo = max(periodo_dt, na.rm = TRUE),
    n_filas = .N,
    n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))
  ),
  by = .(tipo_negocio_raw)
][order(min_periodo, tipo_negocio_raw)]

cat("\nFirst and last period by exact label:\n")
print(tipo_periodo)

# Monthly number of outlets labeled "Estación de servicio", "Bocas de expendio"
# (any category) or something else
boca_mes_tipo <- unique(
  dt[!is.na(periodo_dt) & !is.na(nro_inscripcion),
     .(periodo_dt, nro_inscripcion, tipo_negocio_raw, tipo_negocio_norm)]
)

# The values of grupo_tipo become column names in the wide table below
boca_mes_tipo[, grupo_tipo := fifelse(
  tipo_negocio_norm == "estacion de servicio",
  "Estacion_de_servicio",
  fifelse(grepl("^bocas? de expendio", tipo_negocio_norm),
          "Bocas_de_expendio",
          "Otros")
)]

serie_mes <- boca_mes_tipo[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(periodo_dt, grupo_tipo)
]

total_mes <- boca_mes_tipo[
  , .(total_bocas = uniqueN(nro_inscripcion)),
  by = periodo_dt
]

serie_mes <- total_mes[serie_mes, on = "periodo_dt"]
serie_mes[, share := n_bocas / total_bocas]

serie_mes_wide <- dcast(
  serie_mes,
  periodo_dt + total_bocas ~ grupo_tipo,
  value.var = c("n_bocas", "share"),
  fill = 0
)

setorder(serie_mes_wide, periodo_dt)

cat("\nFirst 24 months of the aggregate series:\n")
print(serie_mes_wide[1:24])

cat("\nLast 24 months of the aggregate series:\n")
print(tail(serie_mes_wide, 24))

# Month from which "Estación de servicio" is gone for good. future_max_est is
# the largest count of that label from each month onwards.
serie_est <- serie_mes_wide[, .(
  periodo_dt,
  n_estacion = n_bocas_Estacion_de_servicio,
  n_boca = n_bocas_Bocas_de_expendio
)]

serie_est[, future_max_est := rev(cummax(rev(n_estacion)))]
cut_station_disappears <- serie_est[future_max_est == 0, min(periodo_dt)]

cat("\nFirst month from which 'Estación de servicio' stays at zero for good:\n")
print(cut_station_disappears)

# First month with any "Bocas de expendio" category
first_boca_period <- serie_est[n_boca > 0, min(periodo_dt)]

cat("\nFirst month with any 'Bocas de expendio' category:\n")
print(first_boca_period)

# Exact labels that take over from "Estación de servicio": monthly composition
# from 2021 onwards and yearly composition over the whole sample
reemplazo_post_2021 <- boca_mes_tipo[
  periodo_dt >= as.IDate("2021-01-01"),
  .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(periodo_dt, tipo_negocio_raw)
][order(periodo_dt, -n_bocas)]

cat("\nMonthly composition by exact label since 2021:\n")
print(reemplazo_post_2021)

reemplazo_anio <- boca_mes_tipo[
  !is.na(periodo_dt),
  .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(anio = year(periodo_dt), tipo_negocio_raw)
][order(anio, -n_bocas)]

cat("\nYearly composition by exact label:\n")
print(reemplazo_anio)

# Boca-months reported with more than one tipo_negocio
multi_tipo_bocames <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    n_tipos = uniqueN(tipo_negocio_raw),
    tipos = paste(sort(unique(tipo_negocio_raw)), collapse = " | ")
  ),
  by = .(periodo_dt, nro_inscripcion)
][n_tipos > 1][order(periodo_dt, nro_inscripcion)]

cat("\nBoca-months with more than one tipo_negocio:\n")
print(nrow(multi_tipo_bocames))

cat("\nFirst cases with more than one tipo_negocio in the same boca-month:\n")
print(multi_tipo_bocames[1:100])

# Stations that go from "Estación de servicio" to a "Bocas de expendio" label.
# safe_min() and safe_max() return NA when a station never carries the label.
safe_min <- function(x) if (all(is.na(x))) as.IDate(NA) else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) as.IDate(NA) else max(x, na.rm = TRUE)

switch_estaciones <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    has_estacion = any(tipo_negocio_norm == "estacion de servicio", na.rm = TRUE),
    has_boca = any(grepl("^bocas? de expendio", tipo_negocio_norm), na.rm = TRUE),
    first_estacion = safe_min(periodo_dt[tipo_negocio_norm == "estacion de servicio"]),
    last_estacion  = safe_max(periodo_dt[tipo_negocio_norm == "estacion de servicio"]),
    first_boca     = safe_min(periodo_dt[grepl("^bocas? de expendio", tipo_negocio_norm)]),
    last_boca      = safe_max(periodo_dt[grepl("^bocas? de expendio", tipo_negocio_norm)])
  ),
  by = nro_inscripcion
]

res_switch <- switch_estaciones[, .(
  n_total_estaciones = .N,
  n_solo_estacion = sum(has_estacion & !has_boca, na.rm = TRUE),
  n_solo_boca = sum(!has_estacion & has_boca, na.rm = TRUE),
  n_ambas = sum(has_estacion & has_boca, na.rm = TRUE)
)]

cat("\nStations by label ever carried (estacion / boca):\n")
print(res_switch)

switch_examples <- switch_estaciones[
  has_estacion == TRUE & has_boca == TRUE
][order(first_estacion, first_boca)]

cat("\nExamples of stations that carry both labels:\n")
print(switch_examples[1:100])

# Type implied by the product mix of each boca-month, as a first look at how
# "Estación de servicio" would be split. Diagnostic only: the labels here are
# written without accents and are not used further; the inference itself is
# done in part 2.
flags_prod <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    has_gnc = any(producto_norm == "gnc", na.rm = TRUE),
    has_glpa = any(producto_norm == "glpa", na.rm = TRUE),
    has_liquid = any(!is.na(producto_norm) & !(producto_norm %chin% c("gnc", "glpa", "n/d", "")))
  ),
  by = .(nro_inscripcion, periodo_dt)
]

flags_prod[, has_prve := FALSE]

if ("canal_norm" %in% names(dt)) {
  prve_tmp <- dt[
    !is.na(periodo_dt) & !is.na(nro_inscripcion),
    .(has_prve = any(grepl("prve", canal_norm), na.rm = TRUE)),
    by = .(nro_inscripcion, periodo_dt)
  ]
  flags_prod[prve_tmp, has_prve := i.has_prve, on = .(nro_inscripcion, periodo_dt)]
}

flags_prod[, tipo_inferido := fifelse(
  has_prve & has_liquid & !has_gnc & !has_glpa,
  "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
  fifelse(
    has_gnc & has_liquid,
    "Bocas de expendio (venta por menor) Duales (liquidos + GNC)",
    fifelse(
      has_glpa & has_liquid,
      "Bocas de expendio (venta por menor) Duales (liquidos + GLPA)",
      fifelse(
        !has_liquid & has_gnc,
        "Bocas de expendio (venta por menor) Solo GNC",
        fifelse(
          !has_liquid & has_glpa,
          "Boca de expendio de solo GLPA",
          fifelse(
            has_liquid,
            "Bocas de expendio (venta por menor) Combustibles liquidos unicamente",
            NA_character_
          )
        )
      )
    )
  )
)]

est_diag <- dt[
  tipo_negocio_norm == "estacion de servicio"
][flags_prod, on = .(nro_inscripcion, periodo_dt)]

est_infer_global <- est_diag[
  , .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = tipo_inferido
][order(-n_boca_mes)]

cat("\nOverall distribution of the inferred type among 'Estación de servicio' observations:\n")
print(est_infer_global)

est_infer_anio <- est_diag[
  , .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = .(anio = year(periodo_dt), tipo_inferido)
][order(anio, -n_boca_mes)]

cat("\nYearly distribution of the inferred type among 'Estación de servicio' observations:\n")
print(est_infer_anio)

# Objects that summarize the diagnosis
cat("\n\nKey objects of the diagnosis:\n")
cat("1) labels_tipo_relevantes\n")
cat("2) tipo_periodo\n")
cat("3) tail(serie_mes_wide, 24)\n")
cat("4) cut_station_disappears\n")
cat("5) reemplazo_anio\n")
cat("6) res_switch\n")
cat("7) est_infer_global\n")
cat("8) tail(est_infer_anio, 20)\n")


# 2. Type inferred month by month ----

# Work on a new copy of the panel loaded in part 1
dt <- copy(eess_all_cleaned5_cut)
setDT(dt)

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

dt[, tipo_negocio_raw := as.character(tipo_negocio)]
dt[, tipo_negocio_norm := norm_txt(tipo_negocio)]
dt[, producto_norm := norm_txt(producto)]

if ("canal_de_comercializacion" %in% names(dt)) {
  dt[, canal_norm := norm_txt(canal_de_comercializacion)]
}

# Product flags by boca-month. Liquids are any product other than GNC, GLPA,
# "n/d" or blank.
flags_prod <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    has_gnc = any(producto_norm == "gnc", na.rm = TRUE),
    has_glpa = any(producto_norm == "glpa", na.rm = TRUE),
    has_liquid = any(!is.na(producto_norm) & !(producto_norm %chin% c("gnc", "glpa", "n/d", "")))
  ),
  by = .(nro_inscripcion, periodo_dt)
]

# PRVE is read from the sales channel, when that column is present
flags_prod[, has_prve := FALSE]

if ("canal_norm" %in% names(dt)) {
  prve_tmp <- dt[
    !is.na(periodo_dt) & !is.na(nro_inscripcion),
    .(has_prve = any(grepl("prve", canal_norm), na.rm = TRUE)),
    by = .(nro_inscripcion, periodo_dt)
  ]
  flags_prod[prve_tmp, has_prve := i.has_prve, on = .(nro_inscripcion, periodo_dt)]
}

# Inferred type by boca-month: liquids + PRVE (no GNC or GLPA), dual liquids +
# GNC, dual liquids + GLPA, GNC only, GLPA only, liquids only. The labels are
# spelled exactly as the detailed categories of the source. Boca-months with
# none of these products stay NA.
flags_prod[, tipo_inferido := fifelse(
  has_prve & has_liquid & !has_gnc & !has_glpa,
  "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
  fifelse(
    has_gnc & has_liquid,
    "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
    fifelse(
      has_glpa & has_liquid,
      "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
      fifelse(
        !has_liquid & has_gnc,
        "Bocas de expendio (venta por menor) Sólo GNC",
        fifelse(
          !has_liquid & has_glpa,
          "Boca de expendio de sólo GLPA",
          fifelse(
            has_liquid,
            "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
            NA_character_
          )
        )
      )
    )
  )
)]

# Attach the inferred type to the panel and use it only for the rows originally
# labeled "Estación de servicio"
dt[flags_prod, tipo_inferido := i.tipo_inferido, on = .(nro_inscripcion, periodo_dt)]

dt[, tipo_negocio_h := tipo_negocio_raw]

dt[
  tipo_negocio_norm == "estacion de servicio" & !is.na(tipo_inferido),
  tipo_negocio_h := tipo_inferido
]

# "Estación de servicio" should be gone from the harmonized variable
check_estacion <- dt[, .(
  n_original_estacion = sum(tipo_negocio_norm == "estacion de servicio", na.rm = TRUE),
  n_post_estacion = sum(norm_txt(tipo_negocio_h) == "estacion de servicio", na.rm = TRUE)
)]

cat("\nCheck of 'Estación de servicio':\n")
print(check_estacion)

# Distribution of the harmonized variable
dist_tipo_h <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = tipo_negocio_h
][order(-n_boca_mes)]

cat("\nDistribution of tipo_negocio_h:\n")
print(dist_tipo_h)

# Where the boca-months originally labeled "Estación de servicio" end up
comp_est <- dt[
  tipo_negocio_norm == "estacion de servicio",
  .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = .(tipo_negocio_raw, tipo_negocio_h)
][order(-n_boca_mes)]

cat("\nWhere the rows originally labeled 'Estación de servicio' end up:\n")
print(comp_est)

# Yearly number of outlets by harmonized type
serie_h_anio <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(anio, tipo_negocio_h)
][order(anio, -n_bocas)]

cat("\nYearly series with tipo_negocio_h:\n")
print(serie_h_anio)


# 3. Save cleaned6: panel without the generic label ----

# dt comes from part 2 and carries tipo_negocio_raw, tipo_negocio_h and the
# helper columns tipo_negocio_norm, producto_norm, canal_norm and tipo_inferido.

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

FILE_OUT <- fs::path(DIR_DATASETS, "eess_all_cleaned6_cut_nostations.rds")

eess_all_cleaned6_cut_nostations <- copy(dt)
setDT(eess_all_cleaned6_cut_nostations)

# tipo_negocio becomes the harmonized variable; the source label stays in
# tipo_negocio_raw
eess_all_cleaned6_cut_nostations[, tipo_negocio := tipo_negocio_h]

drop_cols <- intersect(
  c("tipo_negocio_norm", "producto_norm", "canal_norm", "tipo_inferido"),
  names(eess_all_cleaned6_cut_nostations)
)

if (length(drop_cols) > 0) {
  eess_all_cleaned6_cut_nostations[, (drop_cols) := NULL]
}

# Place tipo_negocio_raw and tipo_negocio_h right after tipo_negocio
if (all(c("tipo_negocio", "tipo_negocio_raw", "tipo_negocio_h") %in% names(eess_all_cleaned6_cut_nostations))) {

  cols_now <- names(eess_all_cleaned6_cut_nostations)

  cols_without_extra <- setdiff(cols_now, c("tipo_negocio_raw", "tipo_negocio_h"))
  pos_tipo <- match("tipo_negocio", cols_without_extra)

  new_order <- append(cols_without_extra,
                      values = c("tipo_negocio_raw", "tipo_negocio_h"),
                      after = pos_tipo)

  setcolorder(eess_all_cleaned6_cut_nostations, new_order)
}

check_cleaned6 <- eess_all_cleaned6_cut_nostations[, .(
  filas = .N,
  columnas = ncol(eess_all_cleaned6_cut_nostations),
  n_tipo_negocio_estacion = sum(norm_txt(tipo_negocio) == "estacion de servicio", na.rm = TRUE),
  n_tipo_negocio_h_estacion = sum(norm_txt(tipo_negocio_h) == "estacion de servicio", na.rm = TRUE),
  n_tipo_negocio_raw_estacion = sum(norm_txt(tipo_negocio_raw) == "estacion de servicio", na.rm = TRUE)
)]

cat("\nFinal check of cleaned6:\n")
print(check_cleaned6)

cat("\nColumn names:\n")
print(names(eess_all_cleaned6_cut_nostations))

saveRDS(eess_all_cleaned6_cut_nostations, FILE_OUT)

cat("\nPanel saved to:\n")
cat(FILE_OUT, "\n")

# Read the file back to verify it
tmp_check <- readRDS(FILE_OUT)
setDT(tmp_check)

cat("\nCheck of the saved file:\n")
cat("Rows:", nrow(tmp_check), "\n")
cat("Columns:", ncol(tmp_check), "\n")

cat("\nRows still labeled 'Estación de servicio' in the key variables:\n")
cat("tipo_negocio:", sum(norm_txt(tmp_check$tipo_negocio) == "estacion de servicio", na.rm = TRUE), "\n")
cat("tipo_negocio_h:", sum(norm_txt(tmp_check$tipo_negocio_h) == "estacion de servicio", na.rm = TRUE), "\n")
cat("tipo_negocio_raw:", sum(norm_txt(tmp_check$tipo_negocio_raw) == "estacion de servicio", na.rm = TRUE), "\n")

rm(tmp_check)
gc()


# 4. Stability of tipo_negocio_h over time ----

# Uses dt from part 2. Because the type is inferred month by month, it changes
# whenever the product mix reported by a station does.
setDT(dt)

# Distinct types per operator over the sample
op_tipos <- dt[
  !is.na(operador) & !is.na(tipo_negocio_h),
  .(
    n_tipos_h = uniqueN(tipo_negocio_h),
    tipos_h = paste(sort(unique(tipo_negocio_h)), collapse = " | ")
  ),
  by = operador
][order(-n_tipos_h, operador)]

cat("\nOperators with more than one tipo_negocio_h in the sample:\n")
print(op_tipos[n_tipos_h > 1][1:100])

cat("\nSummary of n_tipos_h by operator:\n")
print(op_tipos[, .N, by = n_tipos_h][order(n_tipos_h)])

# Distinct types per boca over the sample
boca_tipos <- dt[
  !is.na(nro_inscripcion) & !is.na(tipo_negocio_h),
  .(
    n_tipos_h = uniqueN(tipo_negocio_h),
    tipos_h = paste(sort(unique(tipo_negocio_h)), collapse = " | "),
    first_period = min(periodo_dt, na.rm = TRUE),
    last_period  = max(periodo_dt, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_tipos_h, nro_inscripcion)]

cat("\nBocas with more than one tipo_negocio_h in the sample:\n")
print(boca_tipos[n_tipos_h > 1][1:100])

cat("\nSummary of n_tipos_h by boca:\n")
print(boca_tipos[, .N, by = n_tipos_h][order(n_tipos_h)])

# Distinct types per boca-year, to see whether the type changes too often
boca_anio_tipos <- dt[
  !is.na(nro_inscripcion) & !is.na(tipo_negocio_h),
  .(
    n_tipos_h = uniqueN(tipo_negocio_h),
    tipos_h = paste(sort(unique(tipo_negocio_h)), collapse = " | ")
  ),
  by = .(nro_inscripcion, anio)
][order(-n_tipos_h, nro_inscripcion, anio)]

cat("\nBoca-years with more than one tipo_negocio_h:\n")
print(boca_anio_tipos[n_tipos_h > 1][1:100])

cat("\nSummary of n_tipos_h by boca-year:\n")
print(boca_anio_tipos[, .N, by = n_tipos_h][order(n_tipos_h)])

# Month-by-month history of the first 20 bocas whose type changes
bocas_cambian <- boca_tipos[n_tipos_h > 1, nro_inscripcion]

ej_cambios <- dt[
  nro_inscripcion %in% head(bocas_cambian, 20),
  .(nro_inscripcion, operador, periodo_dt, producto, tipo_negocio_raw, tipo_negocio_h)
][order(nro_inscripcion, periodo_dt, producto)]

cat("\nExamples of bocas whose tipo_negocio_h changes over time:\n")
print(ej_cambios)


# 5. Alternative: type fixed per station (mode over time) ----

# Every station originally labeled "Estación de servicio" gets its most
# frequent monthly type. Start again from the panel on disk; the copies from
# the previous sections are dropped first (each one is about 1 GB).
rm(eess_all_cleaned5_cut, dt, eess_all_cleaned6_cut_nostations)
gc()

FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")

eess_all_cleaned5_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned5_cut)

cat("Panel loaded\n")
cat("Rows:", nrow(eess_all_cleaned5_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned5_cut), "\n")

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

dt <- copy(eess_all_cleaned5_cut)
setDT(dt)

dt[, tipo_negocio_raw  := as.character(tipo_negocio)]
dt[, tipo_negocio_norm := norm_txt(tipo_negocio)]
dt[, producto_norm     := norm_txt(producto)]

if ("canal_de_comercializacion" %in% names(dt)) {
  dt[, canal_norm := norm_txt(canal_de_comercializacion)]
}

# First step: monthly inference, same rule as in part 2
flags_prod <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    has_gnc    = any(producto_norm == "gnc", na.rm = TRUE),
    has_glpa   = any(producto_norm == "glpa", na.rm = TRUE),
    has_liquid = any(!is.na(producto_norm) & !(producto_norm %chin% c("gnc", "glpa", "n/d", "")))
  ),
  by = .(nro_inscripcion, periodo_dt)
]

flags_prod[, has_prve := FALSE]

if ("canal_norm" %in% names(dt)) {
  prve_tmp <- dt[
    !is.na(periodo_dt) & !is.na(nro_inscripcion),
    .(has_prve = any(grepl("prve", canal_norm), na.rm = TRUE)),
    by = .(nro_inscripcion, periodo_dt)
  ]
  flags_prod[prve_tmp, has_prve := i.has_prve, on = .(nro_inscripcion, periodo_dt)]
}

flags_prod[, tipo_inferido_mes := fifelse(
  has_prve & has_liquid & !has_gnc & !has_glpa,
  "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
  fifelse(
    has_gnc & has_liquid,
    "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
    fifelse(
      has_glpa & has_liquid,
      "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
      fifelse(
        !has_liquid & has_gnc,
        "Bocas de expendio (venta por menor) Sólo GNC",
        fifelse(
          !has_liquid & has_glpa,
          "Boca de expendio de sólo GLPA",
          fifelse(
            has_liquid,
            "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
            NA_character_
          )
        )
      )
    )
  )
)]

dt[flags_prod, tipo_inferido_mes := i.tipo_inferido_mes, on = .(nro_inscripcion, periodo_dt)]

dt[, tipo_negocio_h_mes := tipo_negocio_raw]
dt[
  tipo_negocio_norm == "estacion de servicio" & !is.na(tipo_inferido_mes),
  tipo_negocio_h_mes := tipo_inferido_mes
]

# Second step: modal type per station, computed over the boca-months originally
# labeled "Estación de servicio"
est_bocames <- unique(
  dt[tipo_negocio_norm == "estacion de servicio",
     .(nro_inscripcion, periodo_dt, tipo_negocio_h_mes)]
)

# Months with each inferred type, by station
station_type_counts <- est_bocames[
  !is.na(tipo_negocio_h_mes),
  .(n_boca_mes = .N),
  by = .(nro_inscripcion, tipo_negocio_h_mes)
]

# Ties are broken in favor of the types that need specific infrastructure:
# GNC first, then GLPA, then PRVE, then liquids only
priority_map <- data.table(
  tipo_negocio_h_mes = c(
    "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
    "Bocas de expendio (venta por menor) Sólo GNC",
    "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
    "Boca de expendio de sólo GLPA",
    "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
    "Bocas de expendio (venta por menor) Combustibles líquidos únicamente"
  ),
  priority_rank = 1:6
)

# Labels outside the map rank last
station_type_counts[priority_map, priority_rank := i.priority_rank, on = "tipo_negocio_h_mes"]
station_type_counts[is.na(priority_rank), priority_rank := 999L]

station_totals <- est_bocames[
  , .(total_boca_mes_estacion = .N),
  by = nro_inscripcion
]

# Highest month count and number of types tied at the top
station_type_counts[, max_n := max(n_boca_mes), by = nro_inscripcion]
station_type_counts[, n_top_tie := sum(n_boca_mes == max_n), by = nro_inscripcion]

# Sort so that the first row of each station is its most frequent type, with
# ties going to the lower priority_rank
setorder(station_type_counts, nro_inscripcion, -n_boca_mes, priority_rank, tipo_negocio_h_mes)

station_modal <- station_type_counts[
  , .SD[1],
  by = nro_inscripcion
]

station_modal[station_totals, total_boca_mes_estacion := i.total_boca_mes_estacion, on = "nro_inscripcion"]
station_modal[, modal_share_estacion := n_boca_mes / total_boca_mes_estacion]
station_modal[, flag_modal_tie := n_top_tie > 1]

# A modal type covering less than 80% of the months is flagged for inspection;
# the flag does not change the assignment
station_modal[, flag_modal_weak := modal_share_estacion < 0.80]

eess_all_cleaned7_alternative_stationfixed <- copy(dt)
setDT(eess_all_cleaned7_alternative_stationfixed)

# The join is by station, so the modal type goes to every row of the station
eess_all_cleaned7_alternative_stationfixed[
  station_modal,
  `:=`(
    tipo_negocio_h_station = i.tipo_negocio_h_mes,
    modal_share_estacion   = i.modal_share_estacion,
    flag_modal_tie         = i.flag_modal_tie,
    flag_modal_weak        = i.flag_modal_weak
  ),
  on = "nro_inscripcion"
]

# Stations without a modal type keep the source label
eess_all_cleaned7_alternative_stationfixed[, tipo_negocio_h_station := fifelse(
  is.na(tipo_negocio_h_station),
  tipo_negocio_raw,
  tipo_negocio_h_station
)]

eess_all_cleaned7_alternative_stationfixed[, tipo_negocio := tipo_negocio_h_station]

# Stations originally labeled "Estación de servicio" with more than one monthly
# type
station_monthly_stability <- est_bocames[
  , .(n_tipos_h_mes = uniqueN(tipo_negocio_h_mes)),
  by = nro_inscripcion
]

cat("\nStability of the monthly type before fixing it per station:\n")
print(station_monthly_stability[, .N, by = n_tipos_h_mes][order(n_tipos_h_mes)])

# Ties and weak modes
station_modal_summary <- station_modal[, .(
  n_estaciones_raw_estacion = .N,
  n_ties_modal = sum(flag_modal_tie, na.rm = TRUE),
  n_modal_weak = sum(flag_modal_weak, na.rm = TRUE),
  p10_modal_share = as.numeric(quantile(modal_share_estacion, 0.10, na.rm = TRUE)),
  p25_modal_share = as.numeric(quantile(modal_share_estacion, 0.25, na.rm = TRUE)),
  median_modal_share = median(modal_share_estacion, na.rm = TRUE),
  p75_modal_share = as.numeric(quantile(modal_share_estacion, 0.75, na.rm = TRUE)),
  min_modal_share = min(modal_share_estacion, na.rm = TRUE)
)]

cat("\nSummary of the modal type by station:\n")
print(station_modal_summary)

# Boca-months that change when moving from the monthly to the fixed version
compare_mes_vs_station <- eess_all_cleaned7_alternative_stationfixed[
  tipo_negocio_norm == "estacion de servicio",
  .(
    n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))
  ),
  by = .(tipo_negocio_h_mes, tipo_negocio_h_station)
][order(-n_boca_mes)]

cat("\nMonthly version vs station-fixed version:\n")
print(compare_mes_vs_station)

impact_summary <- eess_all_cleaned7_alternative_stationfixed[
  tipo_negocio_norm == "estacion de servicio",
  .(
    n_boca_mes_raw_estacion = uniqueN(paste(nro_inscripcion, periodo_dt)),
    n_boca_mes_cambian_vs_mes = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_mes != tipo_negocio_h_station]),
    share_cambian_vs_mes = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_mes != tipo_negocio_h_station]) /
      uniqueN(paste(nro_inscripcion, periodo_dt))
  )
]

cat("\nImpact of moving from the monthly to the station-fixed version:\n")
print(impact_summary)

# After fixing, each station should be left with a single type
station_fixed_stability <- eess_all_cleaned7_alternative_stationfixed[
  tipo_negocio_norm == "estacion de servicio",
  .(n_tipos_h_station = uniqueN(tipo_negocio_h_station)),
  by = nro_inscripcion
]

cat("\nStability after fixing the type per station:\n")
print(station_fixed_stability[, .N, by = n_tipos_h_station][order(n_tipos_h_station)])

# Cases to inspect: ties or a weak mode
station_cases_to_review <- station_modal[
  flag_modal_tie == TRUE | flag_modal_weak == TRUE
][order(flag_modal_tie, modal_share_estacion, nro_inscripcion)]

cat("\nFirst cases to review (tie or weak mode):\n")
print(station_cases_to_review[1:100])

# Drop helper columns. This version is kept in memory for comparison only and is
# not written to disk.
drop_cols <- intersect(
  c("tipo_negocio_norm", "producto_norm", "canal_norm", "tipo_inferido_mes"),
  names(eess_all_cleaned7_alternative_stationfixed)
)

if (length(drop_cols) > 0) {
  eess_all_cleaned7_alternative_stationfixed[, (drop_cols) := NULL]
}


# 6. Alternatives: "ever" and "since first appearance" ----

# "Ever" infers one type per station from every product it sells at some point
# in the sample. "Since first appearance" switches a feature (GNC, GLPA, PRVE)
# on from the first month the station reports it, so the type does not revert
# when a product is missing in a later month. Start again from the panel on
# disk.
rm(eess_all_cleaned5_cut, dt, eess_all_cleaned7_alternative_stationfixed)
gc()

FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")

eess_all_cleaned5_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned5_cut)

cat("Panel loaded\n")
cat("Rows:", nrow(eess_all_cleaned5_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned5_cut), "\n")

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

dt <- copy(eess_all_cleaned5_cut)
setDT(dt)

dt[, tipo_negocio_raw  := as.character(tipo_negocio)]
dt[, tipo_negocio_norm := norm_txt(tipo_negocio)]
dt[, producto_norm     := norm_txt(producto)]

if ("canal_de_comercializacion" %in% names(dt)) {
  dt[, canal_norm := norm_txt(canal_de_comercializacion)]
}

# Base step: monthly inference, same rule as in part 2
flags_prod <- dt[
  !is.na(periodo_dt) & !is.na(nro_inscripcion),
  .(
    has_gnc    = any(producto_norm == "gnc", na.rm = TRUE),
    has_glpa   = any(producto_norm == "glpa", na.rm = TRUE),
    has_liquid = any(!is.na(producto_norm) & !(producto_norm %chin% c("gnc", "glpa", "n/d", "")))
  ),
  by = .(nro_inscripcion, periodo_dt)
]

flags_prod[, has_prve := FALSE]

if ("canal_norm" %in% names(dt)) {
  prve_tmp <- dt[
    !is.na(periodo_dt) & !is.na(nro_inscripcion),
    .(has_prve = any(grepl("prve", canal_norm), na.rm = TRUE)),
    by = .(nro_inscripcion, periodo_dt)
  ]
  flags_prod[prve_tmp, has_prve := i.has_prve, on = .(nro_inscripcion, periodo_dt)]
}

flags_prod[, tipo_inferido_mes := fifelse(
  has_prve & has_liquid & !has_gnc & !has_glpa,
  "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
  fifelse(
    has_gnc & has_liquid,
    "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
    fifelse(
      has_glpa & has_liquid,
      "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
      fifelse(
        !has_liquid & has_gnc,
        "Bocas de expendio (venta por menor) Sólo GNC",
        fifelse(
          !has_liquid & has_glpa,
          "Boca de expendio de sólo GLPA",
          fifelse(
            has_liquid,
            "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
            NA_character_
          )
        )
      )
    )
  )
)]

dt[flags_prod, tipo_inferido_mes := i.tipo_inferido_mes, on = .(nro_inscripcion, periodo_dt)]

dt[, tipo_negocio_h_mes := tipo_negocio_raw]
dt[
  tipo_negocio_norm == "estacion de servicio" & !is.na(tipo_inferido_mes),
  tipo_negocio_h_mes := tipo_inferido_mes
]

# "Ever": product flags over the whole history of each station that is at some
# point labeled "Estación de servicio"
station_ever <- flags_prod[
  dt[tipo_negocio_norm == "estacion de servicio", .(nro_inscripcion)] |> unique(),
  on = "nro_inscripcion",
  nomatch = 0
][
  , .(
    ever_gnc    = any(has_gnc, na.rm = TRUE),
    ever_glpa   = any(has_glpa, na.rm = TRUE),
    ever_liquid = any(has_liquid, na.rm = TRUE),
    ever_prve   = any(has_prve, na.rm = TRUE)
  ),
  by = nro_inscripcion
]

station_ever[, tipo_negocio_h_ever_station := fifelse(
  ever_prve & ever_liquid & !ever_gnc & !ever_glpa,
  "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
  fifelse(
    ever_gnc & ever_liquid,
    "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
    fifelse(
      ever_glpa & ever_liquid,
      "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
      fifelse(
        !ever_liquid & ever_gnc,
        "Bocas de expendio (venta por menor) Sólo GNC",
        fifelse(
          !ever_liquid & ever_glpa,
          "Boca de expendio de sólo GLPA",
          fifelse(
            ever_liquid,
            "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
            NA_character_
          )
        )
      )
    )
  )
)]

dt[station_ever, tipo_negocio_h_ever_station := i.tipo_negocio_h_ever_station, on = "nro_inscripcion"]

dt[, tipo_negocio_h_ever := tipo_negocio_raw]
dt[
  tipo_negocio_norm == "estacion de servicio" & !is.na(tipo_negocio_h_ever_station),
  tipo_negocio_h_ever := tipo_negocio_h_ever_station
]

# "Since first appearance": first month in which each feature shows up
station_firsts <- flags_prod[
  dt[tipo_negocio_norm == "estacion de servicio", .(nro_inscripcion)] |> unique(),
  on = "nro_inscripcion",
  nomatch = 0
][
  , .(
    first_gnc    = if (any(has_gnc, na.rm = TRUE))    min(periodo_dt[has_gnc == TRUE], na.rm = TRUE)    else as.IDate(NA),
    first_glpa   = if (any(has_glpa, na.rm = TRUE))   min(periodo_dt[has_glpa == TRUE], na.rm = TRUE)   else as.IDate(NA),
    first_liquid = if (any(has_liquid, na.rm = TRUE)) min(periodo_dt[has_liquid == TRUE], na.rm = TRUE) else as.IDate(NA),
    first_prve   = if (any(has_prve, na.rm = TRUE))   min(periodo_dt[has_prve == TRUE], na.rm = TRUE)   else as.IDate(NA)
  ),
  by = nro_inscripcion
]

dt[station_firsts, `:=`(
  first_gnc = i.first_gnc,
  first_glpa = i.first_glpa,
  first_liquid = i.first_liquid,
  first_prve = i.first_prve
), on = "nro_inscripcion"]

dt[, tipo_negocio_h_since := tipo_negocio_raw]

# The rules below run in order and, after the first one, only touch rows that
# still carry the source label:
#   1. liquids and GNC: dual liquids + GNC once both have appeared
#   2. liquids and GLPA: dual liquids + GLPA once both have appeared
#   3. PRVE before any GNC or GLPA: liquids + PRVE once PRVE and liquids have
#      both appeared
#   4. GNC and never liquids: GNC only, from the first GNC month
#   5. GLPA and never liquids: GLPA only, from the first GLPA month
#   6. every remaining row of a station that sells liquids at some point:
#      liquids only

dt[
  tipo_negocio_norm == "estacion de servicio" &
    !is.na(first_gnc) & !is.na(first_liquid) &
    periodo_dt >= pmax(first_gnc, first_liquid),
  tipo_negocio_h_since := "Bocas de expendio (venta por menor) Duales (líquidos + GNC)"
]

dt[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since == tipo_negocio_raw &
    !is.na(first_glpa) & !is.na(first_liquid) &
    periodo_dt >= pmax(first_glpa, first_liquid),
  tipo_negocio_h_since := "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)"
]

dt[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since == tipo_negocio_raw &
    !is.na(first_prve) & !is.na(first_liquid) &
    (is.na(first_gnc) | first_prve < first_gnc) &
    (is.na(first_glpa) | first_prve < first_glpa) &
    periodo_dt >= pmax(first_prve, first_liquid),
  tipo_negocio_h_since := "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE"
]

dt[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since == tipo_negocio_raw &
    !is.na(first_gnc) & is.na(first_liquid) &
    periodo_dt >= first_gnc,
  tipo_negocio_h_since := "Bocas de expendio (venta por menor) Sólo GNC"
]

dt[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since == tipo_negocio_raw &
    !is.na(first_glpa) & is.na(first_liquid) &
    periodo_dt >= first_glpa,
  tipo_negocio_h_since := "Boca de expendio de sólo GLPA"
]

dt[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since == tipo_negocio_raw &
    !is.na(first_liquid),
  tipo_negocio_h_since := "Bocas de expendio (venta por menor) Combustibles líquidos únicamente"
]

eess_all_cleaned7_alternative_sinceappearance <- copy(dt)
setDT(eess_all_cleaned7_alternative_sinceappearance)

# tipo_negocio takes the "since first appearance" version
eess_all_cleaned7_alternative_sinceappearance[, tipo_negocio := tipo_negocio_h_since]

# Changes relative to the monthly version
cmp_mes_since <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio",
  .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = .(tipo_negocio_h_mes, tipo_negocio_h_since)
][order(-n_boca_mes)]

cat("\nMonthly vs since-appearance version:\n")
print(cmp_mes_since)

impact_since <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio",
  .(
    n_boca_mes_raw_estacion = uniqueN(paste(nro_inscripcion, periodo_dt)),
    n_boca_mes_cambian_vs_mes = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_mes != tipo_negocio_h_since]),
    share_cambian_vs_mes = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_mes != tipo_negocio_h_since]) /
      uniqueN(paste(nro_inscripcion, periodo_dt))
  )
]

cat("\nImpact of monthly vs since-appearance:\n")
print(impact_since)

# Changes relative to the "ever" version
cmp_since_ever <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio",
  .(n_boca_mes = uniqueN(paste(nro_inscripcion, periodo_dt))),
  by = .(tipo_negocio_h_since, tipo_negocio_h_ever)
][order(-n_boca_mes)]

cat("\nSince-appearance vs ever version:\n")
print(cmp_since_ever)

impact_ever <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio",
  .(
    n_boca_mes_raw_estacion = uniqueN(paste(nro_inscripcion, periodo_dt)),
    n_boca_mes_cambian_since_vs_ever = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_since != tipo_negocio_h_ever]),
    share_cambian_since_vs_ever = uniqueN(paste(nro_inscripcion, periodo_dt)[tipo_negocio_h_since != tipo_negocio_h_ever]) /
      uniqueN(paste(nro_inscripcion, periodo_dt))
  )
]

cat("\nImpact of since-appearance vs ever:\n")
print(impact_ever)

# Number of types per station under the since-appearance rule
station_since_stability <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio",
  .(n_tipos_h_since = uniqueN(tipo_negocio_h_since)),
  by = nro_inscripcion
]

cat("\nStability by station - since appearance:\n")
print(station_since_stability[, .N, by = n_tipos_h_since][order(n_tipos_h_since)])

# Rows where the two station-level rules disagree
diff_examples <- eess_all_cleaned7_alternative_sinceappearance[
  tipo_negocio_norm == "estacion de servicio" &
    tipo_negocio_h_since != tipo_negocio_h_ever,
  .(nro_inscripcion, operador, periodo_dt, producto, tipo_negocio_raw, tipo_negocio_h_mes, tipo_negocio_h_since, tipo_negocio_h_ever)
][order(nro_inscripcion, periodo_dt, producto)]

cat("\nExamples where since-appearance and ever differ:\n")
print(diff_examples[1:200])

drop_cols <- intersect(
  c("tipo_negocio_norm", "producto_norm", "canal_norm"),
  names(eess_all_cleaned7_alternative_sinceappearance)
)

if (length(drop_cols) > 0) {
  eess_all_cleaned7_alternative_sinceappearance[, (drop_cols) := NULL]
}


# 7. Save cleaned7: tipo_negocio = since-appearance version ----

# Uses eess_all_cleaned7_alternative_sinceappearance from part 6
FILE_OUT <- fs::path(DIR_DATASETS, "eess_all_cleaned7_alternative_sinceappearance.rds")

base_out <- copy(eess_all_cleaned7_alternative_sinceappearance)
setDT(base_out)

base_out[, tipo_negocio := tipo_negocio_h_since]

# Only tipo_negocio_raw and tipo_negocio_h_since are kept next to tipo_negocio;
# the monthly and "ever" versions and the first-appearance dates are dropped
drop_cols <- intersect(
  c(
    "tipo_negocio_norm",
    "producto_norm",
    "canal_norm",
    "tipo_inferido_mes",
    "tipo_negocio_h_mes",
    "tipo_negocio_h_ever",
    "tipo_negocio_h_ever_station",
    "first_gnc",
    "first_glpa",
    "first_liquid",
    "first_prve"
  ),
  names(base_out)
)

if (length(drop_cols) > 0) {
  base_out[, (drop_cols) := NULL]
}

# Place tipo_negocio_raw and tipo_negocio_h_since right after tipo_negocio
cols_now <- names(base_out)

extra_keep <- intersect(
  c("tipo_negocio_raw", "tipo_negocio_h_since"),
  cols_now
)

cols_without_extra <- setdiff(cols_now, extra_keep)
pos_tipo <- match("tipo_negocio", cols_without_extra)

if (!is.na(pos_tipo) && length(extra_keep) > 0) {
  new_order <- append(cols_without_extra, values = extra_keep, after = pos_tipo)
  setcolorder(base_out, new_order)
}

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

check_final <- base_out[, .(
  filas = .N,
  columnas = ncol(base_out),
  n_estacion_en_tipo_negocio = sum(norm_txt(tipo_negocio) == "estacion de servicio", na.rm = TRUE),
  n_estacion_en_raw = sum(norm_txt(tipo_negocio_raw) == "estacion de servicio", na.rm = TRUE),
  n_estacion_en_since = sum(norm_txt(tipo_negocio_h_since) == "estacion de servicio", na.rm = TRUE)
)]

cat("\nFinal check:\n")
print(check_final)

cat("\nColumn names:\n")
print(names(base_out))

saveRDS(base_out, FILE_OUT)

cat("\nPanel saved to:\n")
cat(FILE_OUT, "\n")

# Read the file back to verify it
tmp_check <- readRDS(FILE_OUT)
setDT(tmp_check)

cat("\nCheck of the saved file:\n")
cat("Rows:", nrow(tmp_check), "\n")
cat("Columns:", ncol(tmp_check), "\n")
cat("'Estación de servicio' in tipo_negocio:",
    sum(norm_txt(tmp_check$tipo_negocio) == "estacion de servicio", na.rm = TRUE), "\n")
cat("'Estación de servicio' in tipo_negocio_raw:",
    sum(norm_txt(tmp_check$tipo_negocio_raw) == "estacion de servicio", na.rm = TRUE), "\n")
cat("'Estación de servicio' in tipo_negocio_h_since:",
    sum(norm_txt(tmp_check$tipo_negocio_h_since) == "estacion de servicio", na.rm = TRUE), "\n")

rm(tmp_check)

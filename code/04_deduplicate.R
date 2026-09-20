# 04_deduplicate.R
# Studies records that share the same key (period, registration number, product
# and sales channel), across source files and within a file, and removes the
# copies whose values are identical.
#
# Input:  eess_all_cleaned4_cut.rds
# Output: eess_all_cleaned5_cut.rds
#
# Part 1 only counts and prints. Part 2 does the same work again and deletes,
# reading the panel from disk itself, so it can be run on its own.

library(data.table)
library(fs)

source("code/00_config.R")

# 1. Diagnostics of repeated keys ----

# How often the same key shows up more than once, and whether the copies agree.
#
# Input:  eess_all_cleaned4_cut.rds
# Output: none, this part only prints
#
# There are two ways a key can repeat. The six raw files are split by period and
# two of them cover 2013, so a key can sit in one file and in the next; and a
# single file can carry the key twice on its own. The two cases are counted
# apart, because part 2 deletes them in that order.
#
# For every repeated key the substantive variables are compared. The rule is
# deliberately conservative: a key counts as safe only when each of those
# variables takes a single value across the copies. Anything else is treated as
# a conflicting report and every row is kept. The labels in the printed tables
# name both things at once: solape_entre_archivos_ for the first case,
# mismo_archivo_ for the second, then mismos or distintos valores.

DIR_DATASETS <- DIR_INTERIM
FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned4_cut.rds")

# The panel as script 03 leaves it, with the extreme prices already dropped
eess_all_cleaned4_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned4_cut)

cat("Data loaded\n")
cat("Rows:", nrow(eess_all_cleaned4_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned4_cut), "\n")

# A record is identified by key_vars. Records with the same key are compared on
# substantive_vars: volume, prices, taxes and the other reported amounts.
#
# The Spanish names in that list: excentos flags the tax-exempt part of a sale,
# which the source reports on a line of its own; no_movimientos flags a record
# filed with no activity; and tasa_vial (road levy), ingresos_brutos (provincial
# turnover tax) and fondo_fiduciario_gnc (CNG trust fund) are three of the taxes
# the source reports per record.
key_vars <- c("periodo_dt", "nro_inscripcion", "producto", "canal_de_comercializacion")

substantive_vars <- c(
  "volumen",
  "precio_sin_impuestos",
  "precio_con_impuestos",
  "precio_surtidor",
  "no_movimientos",
  "excentos",
  "impuesto_combustible_liquido",
  "impuesto_dioxido_carbono",
  "tasa_vial",
  "tasa_municipal",
  "ingresos_brutos",
  "iva",
  "fondo_fiduciario_gnc"
)

# 1.1 Overlaps between source files ----

# Keys that appear in more than one source_file. n_obs also counts the times a
# key repeats inside one file, which section 1.3 picks up.
key_source <- eess_all_cleaned4_cut[
  , .(n_obs = .N),
  by = c(key_vars, "source_file")
]

# key_source has one row per key and file, so .N here is the number of files
# the key appears in.
key_source_summary <- key_source[
  , .(
    n_source_files = .N,
    source_files = paste(sort(source_file), collapse = " | "),
    total_obs = sum(n_obs)
  ),
  by = key_vars
][n_source_files > 1]

cat("\nKeys present in more than one source_file:", nrow(key_source_summary), "\n")

# Which pairs of files the overlapping keys come from
overlap_pairs <- key_source_summary[
  , .N,
  by = source_files
][order(-N)]

cat("\nOverlaps by combination of files:\n")
print(overlap_pairs)

key_source_summary[, anio_key := as.integer(format(periodo_dt, "%Y"))]

# And in which years. The overlap falls in 2013, where the 2010-2013 and the
# 2013-2018 raw files cover the same months.
overlap_year <- key_source_summary[
  , .N,
  by = .(anio_key, source_files)
][order(anio_key, -N)]

cat("\nOverlaps by year and combination of files:\n")
print(overlap_year)

# Rows behind those keys. The join brings back every row of the panel that
# carries one of them, from both files.
overlap_rows <- eess_all_cleaned4_cut[key_source_summary, on = key_vars][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

cat("\nRows involved in overlaps between files:", nrow(overlap_rows), "\n")

# 1.2 Values of the overlapping records ----

# Number of distinct values of each substantive variable within a key (NA
# counts as a value). The copies carry the same values when every variable has
# exactly one. A key where one copy reports a number and the other leaves the
# field empty therefore counts as two values, and is not deduplicated.
overlap_compare <- overlap_rows[
  , c(
    list(
      n_obs = .N,
      n_source_files = uniqueN(source_file),
      source_files = paste(sort(unique(source_file)), collapse = " | ")
    ),
    lapply(.SD, uniqueN)
  ),
  by = key_vars,
  .SDcols = substantive_vars
]

setnames(
  overlap_compare,
  old = substantive_vars,
  new = paste0("n_unique_", substantive_vars)
)

uvars <- paste0("n_unique_", substantive_vars)

overlap_compare[
  , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
  .SDcols = uvars
]

# Two outcomes per key: the copies agree everywhere, or they do not. Part 2
# deduplicates the first case and leaves the second alone.
overlap_compare[
  , caso_overlap := fifelse(
    all_equal_substantive,
    "solape_entre_archivos_mismos_valores",
    "solape_entre_archivos_distintos_valores"
  )
]

cat("\nSummary of overlaps between files:\n")
print(overlap_compare[, .N, by = caso_overlap][order(-N)])

# 1.3 Repeated keys within the same source file ----

# The second case: one file reporting the same key twice. Section 1.1 cannot see
# it, because it counts files per key and a repeat inside a file leaves that
# count at one. The comparison of values is the one used above, now run within
# file as well as within key.

key_source_dup <- key_source[n_obs > 1]

cat("\nKeys repeated within the same source_file:", nrow(key_source_dup), "\n")

# Rows behind them, and the count of distinct values of 1.2, now taken by key
# and file
within_rows <- eess_all_cleaned4_cut[key_source_dup, on = c(key_vars, "source_file")][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

within_compare <- within_rows[
  , c(
    list(n_obs = .N),
    lapply(.SD, uniqueN)
  ),
  by = c(key_vars, "source_file"),
  .SDcols = substantive_vars
]

setnames(
  within_compare,
  old = substantive_vars,
  new = paste0("n_unique_", substantive_vars)
)

uvars2 <- paste0("n_unique_", substantive_vars)

within_compare[
  , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
  .SDcols = uvars2
]

within_compare[
  , caso_within := fifelse(
    all_equal_substantive,
    "mismo_archivo_mismos_valores",
    "mismo_archivo_distintos_valores"
  )
]

cat("\nSummary within the same file:\n")
print(within_compare[, .N, by = caso_within][order(-N)])

# 1.4 Summary of the diagnostics ----

# Both cases side by side: repeated keys and the rows behind them.

resumen_dup_v3 <- data.table(
  filas_originales = nrow(eess_all_cleaned4_cut),
  keys_en_mas_de_un_source = nrow(key_source_summary),
  filas_solapadas_entre_archivos = nrow(overlap_rows),
  keys_repetidas_dentro_mismo_source = nrow(key_source_dup),
  filas_repetidas_dentro_mismo_source = nrow(within_rows)
)

cat("\nOverall summary:\n")
print(resumen_dup_v3)

# 2. Deduplication ----

# The panel is read again from disk and the comparisons of part 1 are rebuilt,
# this time keeping the keys that are safe to deduplicate: those whose copies
# carry the same values. Keys with different values are left as they are.
#
# Input:  eess_all_cleaned4_cut.rds
# Output: eess_all_cleaned5_cut.rds
#
# Deletion runs in two steps, copies between files first and copies within a
# file second, with the within-file comparison recomputed on what the first step
# leaves. What survives is every pair of rows that disagrees somewhere: the
# taxed and the tax-exempt line of one sale differ in excentos, so they are
# never treated as copies, and neither are the genuinely conflicting reports.
library(data.table)
library(fs)

DIR_DATASETS <- DIR_INTERIM
FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned4_cut.rds")
FILE_OUT  <- fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")

if (!fs::file_exists(FILE_BASE)) {
  stop("File not found: ", FILE_BASE)
}

eess_all_cleaned4_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned4_cut)

cat("Data loaded\n")
cat("Rows:", nrow(eess_all_cleaned4_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned4_cut), "\n")

# Key and comparison variables, as defined in part 1
key_vars <- c("periodo_dt", "nro_inscripcion", "producto", "canal_de_comercializacion")

substantive_vars <- c(
  "volumen",
  "precio_sin_impuestos",
  "precio_con_impuestos",
  "precio_surtidor",
  "no_movimientos",
  "excentos",
  "impuesto_combustible_liquido",
  "impuesto_dioxido_carbono",
  "tasa_vial",
  "tasa_municipal",
  "ingresos_brutos",
  "iva",
  "fondo_fiduciario_gnc"
)

# Stop before deleting anything if the panel is missing a key column, one of the
# variables the comparison rests on, or source_file
stopifnot(all(key_vars %in% names(eess_all_cleaned4_cut)))
stopifnot(all(substantive_vars %in% names(eess_all_cleaned4_cut)))
stopifnot("source_file" %in% names(eess_all_cleaned4_cut))

# 2.1 Overlaps between source files ----

# Sections 1.1 and 1.2 again, on the panel as it was just loaded, ending in the
# list of keys whose copies agree on every substantive variable.

key_source <- eess_all_cleaned4_cut[
  , .(n_obs = .N),
  by = c(key_vars, "source_file")
]

key_source_summary <- key_source[
  , .(
    n_source_files = .N,
    source_files = paste(sort(source_file), collapse = " | "),
    total_obs = sum(n_obs)
  ),
  by = key_vars
][n_source_files > 1]

cat("\nKeys present in more than one source_file:", nrow(key_source_summary), "\n")

overlap_rows <- eess_all_cleaned4_cut[key_source_summary, on = key_vars][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

overlap_compare <- overlap_rows[
  , c(
    list(
      n_obs = .N,
      n_source_files = uniqueN(source_file),
      source_files = paste(sort(unique(source_file)), collapse = " | ")
    ),
    lapply(.SD, uniqueN)
  ),
  by = key_vars,
  .SDcols = substantive_vars
]

setnames(
  overlap_compare,
  old = substantive_vars,
  new = paste0("n_unique_", substantive_vars)
)

uvars <- paste0("n_unique_", substantive_vars)

overlap_compare[
  , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
  .SDcols = uvars
]

overlap_compare[
  , caso_overlap := fifelse(
    all_equal_substantive,
    "solape_entre_archivos_mismos_valores",
    "solape_entre_archivos_distintos_valores"
  )
]

cat("\nSummary of overlaps between files:\n")
print(overlap_compare[, .N, by = caso_overlap][order(-N)])

# Keys safe to deduplicate between files
overlap_safe_keys <- overlap_compare[
  caso_overlap == "solape_entre_archivos_mismos_valores",
  ..key_vars
]

cat("\nKeys safe to deduplicate between files:", nrow(overlap_safe_keys), "\n")

# 2.2 Repeated keys within the same source file ----

# Section 1.3 again. The keys it finds are reported but not acted on here: step 2
# rebuilds the same comparison on the panel left after step 1, since dropping
# the cross-file copies changes which keys are still repeated.

key_source_dup <- key_source[n_obs > 1]

within_rows <- eess_all_cleaned4_cut[key_source_dup, on = c(key_vars, "source_file")][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

within_compare <- within_rows[
  , c(
    list(n_obs = .N),
    lapply(.SD, uniqueN)
  ),
  by = c(key_vars, "source_file"),
  .SDcols = substantive_vars
]

setnames(
  within_compare,
  old = substantive_vars,
  new = paste0("n_unique_", substantive_vars)
)

uvars2 <- paste0("n_unique_", substantive_vars)

within_compare[
  , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
  .SDcols = uvars2
]

within_compare[
  , caso_within := fifelse(
    all_equal_substantive,
    "mismo_archivo_mismos_valores",
    "mismo_archivo_distintos_valores"
  )
]

cat("\nSummary within the same file:\n")
print(within_compare[, .N, by = caso_within][order(-N)])

# Keys safe to deduplicate within the same file
within_safe_keys <- within_compare[
  caso_within == "mismo_archivo_mismos_valores",
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

cat("\nKeys safe to deduplicate within the same file:", nrow(within_safe_keys), "\n")

# 2.3 Step 1: copies between files ----

# Rows to drop are identified by a row id instead of fsetdiff(), which works on
# unique rows and so cannot single out one copy of a pair
eess_all_cleaned4_cut[, row_id := .I]

overlap_safe_rows <- eess_all_cleaned4_cut[overlap_safe_keys, on = key_vars][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file, row_id)
]

# Within each safe key the first row is kept and the rest are dropped. Rows are
# sorted by source_file and row_id, so the row kept comes from the file whose
# name sorts first. The copies agree on every substantive variable, so the
# choice only shows up in the columns outside that list, source_file among them.
overlap_rows_to_drop <- overlap_safe_rows[
  , .SD[-1],
  by = key_vars
]

cat("\nRows to drop, safe overlap between files:", nrow(overlap_rows_to_drop), "\n")

base_step1 <- eess_all_cleaned4_cut[!row_id %in% overlap_rows_to_drop$row_id]

cat("Rows after step 1:", nrow(base_step1), "\n")

# 2.4 Step 2: copies within the same file ----

# The within-file comparison is redone on the panel left after step 1, which no
# longer has the cross-file copies. Keys that were repeated only because of
# those copies are now single records and drop out of the count.
key_source_step1 <- base_step1[
  , .(n_obs = .N),
  by = c(key_vars, "source_file")
]

key_source_dup_step1 <- key_source_step1[n_obs > 1]

within_rows_step1 <- base_step1[key_source_dup_step1, on = c(key_vars, "source_file")][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file, row_id)
]

within_compare_step1 <- within_rows_step1[
  , c(
    list(n_obs = .N),
    lapply(.SD, uniqueN)
  ),
  by = c(key_vars, "source_file"),
  .SDcols = substantive_vars
]

setnames(
  within_compare_step1,
  old = substantive_vars,
  new = paste0("n_unique_", substantive_vars)
)

uvars3 <- paste0("n_unique_", substantive_vars)

within_compare_step1[
  , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
  .SDcols = uvars3
]

# Keys whose remaining rows still agree on every substantive variable
within_safe_keys_step1 <- within_compare_step1[
  all_equal_substantive == TRUE,
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

cat("Keys safe to deduplicate within the same file (after step 1):", nrow(within_safe_keys_step1), "\n")

within_safe_rows_step1 <- base_step1[within_safe_keys_step1, on = c(key_vars, "source_file")][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file, row_id)
]

# Same rule as in step 1, with the file in the grouping: the first row of each
# safe key stays and the other copies are marked for deletion.
within_rows_to_drop <- within_safe_rows_step1[
  , .SD[-1],
  by = c(key_vars, "source_file")
]

cat("Rows to drop, safe duplicate within the same file:", nrow(within_rows_to_drop), "\n")

# 2.5 Deduplicated panel ----

# Drop what step 2 marked and remove the row id, which was only needed to
# address individual rows.

eess_all_cleaned5_cut <- base_step1[!row_id %in% within_rows_to_drop$row_id]

eess_all_cleaned5_cut[, row_id := NULL]

cat("\nFinal rows in cleaned5:", nrow(eess_all_cleaned5_cut), "\n")
cat("Final columns in cleaned5:", ncol(eess_all_cleaned5_cut), "\n")

# 2.6 Final checks ----

# Both checks rebuild the comparison on the deduplicated panel and count the
# keys that would still qualify as safe. Both should come out at zero. Keys
# whose rows disagree are expected to remain, and they are not counted here.

# Check A: no safe overlaps between files should be left
key_source_final <- eess_all_cleaned5_cut[
  , .(n_obs = .N),
  by = c(key_vars, "source_file")
]

key_source_summary_final <- key_source_final[
  , .(
    n_source_files = .N
  ),
  by = key_vars
][n_source_files > 1]

# The comparison is only rebuilt if some key is still in two files; with none
# left there is nothing to count
if (nrow(key_source_summary_final) > 0) {
  overlap_rows_final <- eess_all_cleaned5_cut[key_source_summary_final, on = key_vars]

  overlap_compare_final <- overlap_rows_final[
    , c(
      list(n_obs = .N),
      lapply(.SD, uniqueN)
    ),
    by = key_vars,
    .SDcols = substantive_vars
  ]

  setnames(
    overlap_compare_final,
    old = substantive_vars,
    new = paste0("n_unique_", substantive_vars)
  )

  uvars_final <- paste0("n_unique_", substantive_vars)

  overlap_compare_final[
    , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
    .SDcols = uvars_final
  ]

  n_overlap_safe_left <- overlap_compare_final[all_equal_substantive == TRUE, .N]
} else {
  n_overlap_safe_left <- 0L
}

cat("\nFinal check - safe overlaps between files left:", n_overlap_safe_left, "\n")

# Check B: no safe duplicates within the same file should be left
key_source_dup_final <- key_source_final[n_obs > 1]

if (nrow(key_source_dup_final) > 0) {
  within_rows_final <- eess_all_cleaned5_cut[key_source_dup_final, on = c(key_vars, "source_file")]

  within_compare_final <- within_rows_final[
    , c(
      list(n_obs = .N),
      lapply(.SD, uniqueN)
    ),
    by = c(key_vars, "source_file"),
    .SDcols = substantive_vars
  ]

  setnames(
    within_compare_final,
    old = substantive_vars,
    new = paste0("n_unique_", substantive_vars)
  )

  uvars_final2 <- paste0("n_unique_", substantive_vars)

  within_compare_final[
    , all_equal_substantive := rowSums(.SD != 1, na.rm = TRUE) == 0,
    .SDcols = uvars_final2
  ]

  n_within_safe_left <- within_compare_final[all_equal_substantive == TRUE, .N]
} else {
  n_within_safe_left <- 0L
}

cat("Final check - safe duplicates within the same file left:", n_within_safe_left, "\n")

# 2.7 Save ----

# The panel is written, summarized, and written again to the same path after
# dropping rowid_key, an auxiliary column it may still carry from an earlier
# step.

saveRDS(eess_all_cleaned5_cut, FILE_OUT)

cat("\nData saved to:\n")
cat(FILE_OUT, "\n")

# Keys and rows behind each of the two steps, next to the row counts in and out
resumen_cleaned5 <- data.table(
  filas_cleaned4 = nrow(eess_all_cleaned4_cut),
  keys_solape_seguro_entre_archivos = nrow(overlap_safe_keys),
  filas_eliminadas_por_solape_entre_archivos = nrow(overlap_rows_to_drop),
  keys_seguras_dentro_mismo_archivo_post_step1 = nrow(within_safe_keys_step1),
  filas_eliminadas_dentro_mismo_archivo = nrow(within_rows_to_drop),
  filas_cleaned5 = nrow(eess_all_cleaned5_cut)
)

cat("\nFinal summary:\n")
print(resumen_cleaned5)

# Drop a leftover auxiliary column, if present, and save again
if ("rowid_key" %in% names(eess_all_cleaned5_cut)) {
  eess_all_cleaned5_cut[, rowid_key := NULL]
}

cat("Rows:", nrow(eess_all_cleaned5_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned5_cut), "\n")
print(names(eess_all_cleaned5_cut))

saveRDS(
  eess_all_cleaned5_cut,
  fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")
)

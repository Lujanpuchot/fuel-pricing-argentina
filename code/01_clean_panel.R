# 01_clean_panel.R
# Builds the analysis panel from the six raw files reported by fuel outlets:
# binds them, cleans volume and prices, removes duplicate records and infers
# the type of outlet. Each part reads the file written by the previous one, so
# the script can be restarted at any part.
#
# Part 6 assigns every locality to a department, which is the market of the
# demand model, and adds that column to the panel.
#
# Input:  public_vi_access_eess_*.rds (six raw files in DIR_RETAIL)
# Output: the eess_all_* chain in DIR_INTERIM. The analysis panel is
#         eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
#
# How the thresholds below were chosen is documented in
# diagnostics_data_quality.R, which does not modify the panel.

suppressPackageStartupMessages({
  library(fs)
  library(data.table)
  library(openxlsx)
  library(jsonlite)
})

source("code/00_config.R")

DIR_RAW_MIN  <- DIR_RETAIL
DIR_DATASETS <- DIR_INTERIM
DIR_INT      <- DIR_INTERIM
fs::dir_create(DIR_DATASETS)

# Part 1. Bind the raw files ----

# Binds the six raw retail files (station-level prices and volumes) into one
# panel, records which columns each file has, builds a monthly date and cleans
# the column names.
#
# Input:  public_vi_access_eess_*.rds (six raw files in DIR_RETAIL)
# Output: eess_all_rawbind.rds (plain bind), eess_all_rawbind_2.rds (with date
#         variables and clean column names) and
#         schema_y_variables_rawfiles.xlsx, all in DIR_INTERIM

# Paths ----

# Raw retail files, one per period
FILES_MINORISTAS_RAW <- c(
  "public_vi_access_eess_menor_2007.rds",
  "public_vi_access_eess_2007_2009.rds",
  "public_vi_access_eess_2010_2013.rds",
  "public_vi_access_eess_2013_2018.rds",
  "public_vi_access_eess_2018_2022.rds",
  "public_vi_access_eess_2022_2024.rds"
)

PATHS_MINORISTAS_RAW <- fs::path(DIR_RAW_MIN, FILES_MINORISTAS_RAW)

# Stop if any raw file is missing
missing <- PATHS_MINORISTAS_RAW[!fs::file_exists(PATHS_MINORISTAS_RAW)]
if (length(missing) > 0) {
  stop("Raw files missing from 'Última versión':\n- ",
       paste(fs::path_file(missing), collapse = "\n- "))
}

message("ROOT = ", ROOT)
message("DIR_RAW_MIN = ", DIR_RAW_MIN)
message("DIR_DATASETS = ", DIR_DATASETS)

# Read the raw files ----

# source_file records the file each row comes from; 04_deduplicate.R uses it to
# study overlaps between files.
read_one_dt <- function(p) {
  x <- readRDS(p)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  data.table::setDT(x)
  x[, source_file := basename(p)]
  x
}

# Each file is kept as a separate element of lst
lst <- vector("list", length(PATHS_MINORISTAS_RAW))
names(lst) <- fs::path_file(PATHS_MINORISTAS_RAW)

for (i in seq_along(PATHS_MINORISTAS_RAW)) {
  p <- PATHS_MINORISTAS_RAW[i]
  x <- read_one_dt(p)
  lst[[i]] <- x

  cat("\n----------------------------\n")
  cat("File:", names(lst)[i], "\n")
  cat("Rows:", nrow(x), " | Cols:", ncol(x), "\n")
  cat("Variables:\n")
  cat(paste(names(x), collapse = ", "), "\n")
}

schema_summary <- data.table::rbindlist(lapply(names(lst), function(f) {
  data.table::data.table(
    file = f,
    nrows = nrow(lst[[f]]),
    ncols = ncol(lst[[f]])
  )
}))

print(schema_summary[order(file)])

vars_by_file <- lapply(lst, names)

# Example: columns of the 2013-2018 file
vars_by_file[["public_vi_access_eess_2013_2018.rds"]]

# Column inventory saved to Excel ----

# Long format: one row per (file, variable)
vars_long <- data.table::rbindlist(lapply(names(vars_by_file), function(f) {
  data.table::data.table(file = f, variable = vars_by_file[[f]])
}))

# Compact format: one row per file, all its variables in one cell
vars_compact <- data.table::data.table(
  file = names(vars_by_file),
  variables = vapply(vars_by_file, function(v) paste(v, collapse = ", "), character(1))
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "schema_summary")
openxlsx::writeData(wb, "schema_summary", schema_summary)

openxlsx::addWorksheet(wb, "vars_long")
openxlsx::writeData(wb, "vars_long", vars_long)

openxlsx::addWorksheet(wb, "vars_compact")
openxlsx::writeData(wb, "vars_compact", vars_compact)

xlsx_path <- fs::path(DIR_DATASETS, "schema_y_variables_rawfiles.xlsx")
openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

message("Excel file saved to: ", xlsx_path)

# Columns common to all files ----

vars_by_file <- lapply(lst, names)

common_vars <- Reduce(intersect, vars_by_file)

extras_by_file <- lapply(vars_by_file, function(v) setdiff(v, common_vars))
missing_by_file <- lapply(vars_by_file, function(v) setdiff(common_vars, v))

cat("Columns common to all files:", length(common_vars), "\n\n")

cat("Extra columns by file:\n")
print(lapply(extras_by_file, sort))

cat("\nMissing columns (relative to the common set):\n")
print(lapply(missing_by_file, sort))

# Bind the six files ----

# fill = TRUE leaves NA in the older files for the seven columns that only the
# newer files have.
eess_all <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)

extra_cols <- setdiff(names(eess_all), common_vars)
cat("Extra columns in the bind:", paste(extra_cols, collapse = ", "), "\n")

out_bind <- fs::path(DIR_DATASETS, "eess_all_rawbind.rds")
saveRDS(eess_all, out_bind, compress = "gzip")
cat("Saved to:", out_bind, "\n")

colnames(eess_all)

# Date variable ----

unique(eess_all$Período)

# Describes how Período is stored in one file: class, blanks, separators found
# and the share of values that match each date pattern.
diag_periodo <- function(dt, file, n_examples = 15) {
  per <- dt[["Período"]]

  cls <- paste(class(per), collapse = "|")
  n   <- length(per)

  # Already parsed as a date: report the range and return
  if (inherits(per, c("Date", "IDate", "POSIXct", "POSIXt"))) {
    rng <- tryCatch(range(per, na.rm = TRUE), error = function(e) c(NA, NA))
    return(list(
      file = file, class = cls, n = n,
      note = "Already a date (Date/IDate/POSIXct), nothing to parse.",
      range = rng
    ))
  }

  per_chr  <- as.character(per)
  per_trim <- trimws(per_chr)

  n_na    <- sum(is.na(per_chr))
  n_blank <- sum(!is.na(per_trim) & per_trim == "")
  n_trim_changed <- sum(!is.na(per_chr) & per_chr != per_trim)

  # Separators present, as a rough indication of the format
  n_has_slash <- sum(grepl("/", per_trim, fixed = TRUE), na.rm = TRUE)
  n_has_dash  <- sum(grepl("-", per_trim, fixed = TRUE), na.rm = TRUE)
  n_has_dot   <- sum(grepl(".", per_trim, fixed = TRUE), na.rm = TRUE)

  # Classify each value by regex; the categories are mutually exclusive
  fmt <- rep("OTHER/UNKNOWN", n)

  fmt[is.na(per_trim) | per_trim == ""] <- "MISSING/BLANK"

  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  # Date with time
  fmt[idx & grepl("^\\d{4}[-/]\\d{2}[-/]\\d{2}\\s+\\d{2}:\\d{2}(:\\d{2})?$", per_trim)] <- "YYYY/MM/DD hh:mm[:ss]"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  # Full date
  fmt[idx & grepl("^\\d{4}[-/]\\d{2}[-/]\\d{2}$", per_trim)] <- "YYYY/MM/DD"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  # Year-month, zero-padded
  fmt[idx & grepl("^\\d{4}[-/]\\d{2}$", per_trim)] <- "YYYY/MM"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  # Not zero-padded (one-digit month or day)
  fmt[idx & grepl("^\\d{4}[-/]\\d{1,2}$", per_trim)] <- "YYYY/M (no pad)"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  fmt[idx & grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}$", per_trim)] <- "YYYY/M/D (no pad)"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  # Digits only
  fmt[idx & grepl("^\\d{6}$", per_trim)] <- "YYYYMM (digits)"
  idx <- fmt == "OTHER/UNKNOWN" & !is.na(per_trim)

  fmt[idx & grepl("^\\d{8}$", per_trim)] <- "YYYYMMDD (digits)"

  fmt_tab <- data.table::as.data.table(table(fmt))
  data.table::setnames(fmt_tab, c("format", "N"))
  fmt_tab[, share := round(N / n, 4)]
  data.table::setorder(fmt_tab, -N)

  # Examples of values that match none of the patterns
  weird_examples <- unique(per_trim[fmt == "OTHER/UNKNOWN"])[1:n_examples]
  weird_examples <- weird_examples[!is.na(weird_examples)]

  # String length
  nch <- nchar(per_trim)
  nch_summary <- c(
    min = suppressWarnings(min(nch, na.rm = TRUE)),
    p50 = suppressWarnings(stats::median(nch, na.rm = TRUE)),
    max = suppressWarnings(max(nch, na.rm = TRUE))
  )

  list(
    file = file, class = cls, n = n,
    n_na = n_na, n_blank = n_blank, n_trim_changed = n_trim_changed,
    sep_counts = c(has_slash = n_has_slash, has_dash = n_has_dash, has_dot = n_has_dot),
    nch_summary = nch_summary,
    fmt_tab = fmt_tab,
    weird_examples = weird_examples
  )
}

# One report per raw file
reports <- lapply(names(lst), function(f) diag_periodo(lst[[f]], f))
names(reports) <- names(lst)

for (f in names(reports)) {
  r <- reports[[f]]
  cat("\n----------------------------------------------------\n")
  cat("File:", r$file, "\n")
  cat("Class:", r$class, " | N:", r$n, "\n")

  if (!is.null(r$note)) {
    cat("Note:", r$note, "\n")
    cat("Range:", paste(r$range, collapse = " - "), "\n")
    next
  }

  cat("NA:", r$n_na, " | blank:", r$n_blank, " | trim-changed:", r$n_trim_changed, "\n")
  cat("Separators (number of rows containing each):",
      " / =", r$sep_counts["has_slash"],
      " - =", r$sep_counts["has_dash"],
      " . =", r$sep_counts["has_dot"], "\n")
  cat("nchar(Período) min/med/max:", paste(r$nch_summary, collapse = " / "), "\n\n")

  print(r$fmt_tab)

  if (length(r$weird_examples) > 0) {
    cat("\nFirst 'OTHER/UNKNOWN' examples:\n")
    print(r$weird_examples)
  } else {
    cat("\nNo values outside the expected formats.\n")
  }
}

# Every value of Período must be YYYY/MM; stop otherwise
bad <- eess_all[!grepl("^\\d{4}/\\d{2}$", `Período`)]
if (nrow(bad) > 0) {
  print(unique(bad$`Período`)[1:50])
  stop("Some values of `Período` are not YYYY/MM. See the values printed above.")
}

# Monthly date, set to the first day of the month
eess_all[, periodo_dt := data.table::as.IDate(
  paste0(`Período`, "/01"),
  format = "%Y/%m/%d"
)]

stopifnot(eess_all[is.na(periodo_dt), .N] == 0)

print(range(eess_all$periodo_dt))

# Months missing between the first and the last period ----

u <- sort(unique(as.Date(eess_all$periodo_dt)))
all_months <- seq(min(u), max(u), by = "month")
missing_months <- setdiff(all_months, u)

if (length(missing_months) > 0) {
  cat("Missing months:\n")
  print(format(missing_months, "%Y/%m"))
} else {
  cat("OK: no months missing between min and max.\n")
}

# Year and month columns ----

eess_all[, `:=`(
  anio = as.integer(substr(`Período`, 1, 4)),
  mes  = as.integer(substr(`Período`, 6, 7))
)]

# Column names ----

# Lower case, accents removed, runs of any other character replaced by one
# underscore. "Período" becomes "periodo".
old <- names(eess_all)
new <- iconv(old, from = "", to = "ASCII//TRANSLIT")
new <- tolower(new)
new <- gsub("[^a-z0-9]+", "_", new)
new <- gsub("_+", "_", new)
new <- gsub("^_|_$", "", new)
new <- make.unique(new, sep = "_")
setnames(eess_all, old, new)

# Date columns first
first <- c("periodo", "anio", "mes", "periodo_dt")
setcolorder(eess_all, c(first, setdiff(names(eess_all), first)))

# Save the panel with clean names and the new column order
saveRDS(
  eess_all,
  fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"),
  compress = "gzip"
)

message("Saved to: ", fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"))

# Part 2. Volume ----

# Diagnoses the raw volume variable, parses it to numeric, sets implausibly large
# volumes (GNC and liquid fuels) to NA and drops rows with missing or tiny volume.
#
# Input:  eess_all_rawbind_2.rds
# Output: eess_all_cleaned1.rds, eess_all_working_with_aux.rds,
#         eess_all_cleaned2.rds, eess_all_cleaned2_cut.rds,
#         eess_all_cleaned3_cut.rds

# Panel saved by 01_bind_raw.R
eess_all <- readRDS(fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"))
setDT(eess_all)

# 2.1 Raw volumen: diagnostics ----

cat("\nInitial diagnostics of volumen\n")
cat("Class of 'volumen': ", class(eess_all$volumen), "\n")
cat("Rows: ", nrow(eess_all), "\n")
cat("NA in volumen (raw): ", sum(is.na(eess_all$volumen)), "\n")

# Trimmed character copy, used for the diagnostics and for parsing
vol_chr <- trimws(as.character(eess_all$volumen))

cat("Blank strings (''): ", sum(!is.na(vol_chr) & vol_chr == ""), "\n")
cat("Count of 'N/D': ", sum(vol_chr == "N/D", na.rm = TRUE), "\n")
cat("Count of 'ND': ", sum(vol_chr == "ND", na.rm = TRUE), "\n")
cat("Count of '-': ", sum(vol_chr == "-", na.rm = TRUE), "\n")

cat("\nFirst unique values of volumen:\n")
print(head(unique(vol_chr), 40))

cat("\nFormats and separators:\n")
cat("With comma: ", sum(grepl(",", vol_chr, fixed = TRUE), na.rm = TRUE), "\n")
cat("With period: ", sum(grepl(".", vol_chr, fixed = TRUE), na.rm = TRUE), "\n")
cat("With comma and period: ",
    sum(grepl(",", vol_chr, fixed = TRUE) & grepl(".", vol_chr, fixed = TRUE), na.rm = TRUE), "\n")
cat("With inner spaces: ", sum(grepl(" ", vol_chr, fixed = TRUE), na.rm = TRUE), "\n")

cat("\nString length (nchar):\n")
print(summary(nchar(vol_chr)))

cat("\nMost frequent raw values of volumen:\n")
print(head(sort(table(vol_chr), decreasing = TRUE), 30))

# 2.2 Parse to numeric ----

# volumen is stored as character, but in a format that as.numeric() reads
# directly (e.g. "21.9920000", "0E-7"), so no custom parser is needed.
eess_all[, volumen_num := suppressWarnings(as.numeric(vol_chr))]

# 2.3 Diagnostics of the parsed variable ----

cat("\nAfter conversion to numeric\n")
cat("Class of 'volumen_num': ", class(eess_all$volumen_num), "\n")
cat("NA in volumen_num: ", sum(is.na(eess_all$volumen_num)), "\n")

# Strings that failed to parse and are not an obvious placeholder
vol_fail <- eess_all[
  !is.na(vol_chr) &
    vol_chr != "" &
    !(vol_chr %in% c("N/D", "ND", "-", "NA", "NULL")) &
    is.na(volumen_num),
  .(volumen)
]

cat("Unparsed values (other than obvious placeholders): ", nrow(vol_fail), "\n")
if (nrow(vol_fail) > 0) {
  cat("\nExamples of unparsed values:\n")
  print(head(unique(vol_fail$volumen), 50))
}

# Pooled statistics. All products are mixed here, so they only validate the
# conversion and carry no economic meaning.
cat("\nPooled summary statistics\n")
print(summary(eess_all$volumen_num))

cat("\nPooled percentiles of volumen_num:\n")
print(quantile(
  eess_all$volumen_num,
  probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
  na.rm = TRUE
))

cat("\nBasic counts:\n")
cat("Volume = 0: ", eess_all[volumen_num == 0, .N], "\n")
cat("Volume < 0: ", eess_all[volumen_num < 0, .N], "\n")
cat("Volume > 0: ", eess_all[volumen_num > 0, .N], "\n")
cat("Volume NA: ", eess_all[is.na(volumen_num), .N], "\n")

cat("\nExtremes of volumen_num\n")

cat("\n20 largest values:\n")
print(
  eess_all[!is.na(volumen_num)][
    order(-volumen_num),
    .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num, source_file)
  ][1:20]
)

cat("\n20 smallest non-negative values:\n")
print(
  eess_all[!is.na(volumen_num) & volumen_num >= 0][
    order(volumen_num),
    .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num, source_file)
  ][1:20]
)

cat("\nNegative values (if any):\n")
print(
  eess_all[
    !is.na(volumen_num) & volumen_num < 0,
    .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num, source_file)
  ][1:20]
)

# By month
volumen_diag_mes <- eess_all[
  , .(
    n = .N,
    n_na_volumen = sum(is.na(volumen_num)),
    share_na_volumen = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p90 = quantile(volumen_num, 0.90, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(periodo_dt)
][order(periodo_dt)]

# max() returns -Inf for a group with no valid volume; non-finite statistics
# are set to NA
volumen_diag_mes[!is.finite(p50), p50 := NA_real_]
volumen_diag_mes[!is.finite(p90), p90 := NA_real_]
volumen_diag_mes[!is.finite(p99), p99 := NA_real_]
volumen_diag_mes[!is.finite(max_vol), max_vol := NA_real_]

cat("\nVolume by month (first months):\n")
print(head(volumen_diag_mes, 20))

cat("\nVolume by month (last months):\n")
print(tail(volumen_diag_mes, 20))

# By product, the breakdown that is economically meaningful
volumen_diag_producto <- eess_all[
  , .(
    n = .N,
    n_na_volumen = sum(is.na(volumen_num)),
    share_na_volumen = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p1 = quantile(volumen_num, 0.01, na.rm = TRUE),
    p5 = quantile(volumen_num, 0.05, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto)
][order(-n)]

volumen_diag_producto[!is.finite(p1), p1 := NA_real_]
volumen_diag_producto[!is.finite(p5), p5 := NA_real_]
volumen_diag_producto[!is.finite(p50), p50 := NA_real_]
volumen_diag_producto[!is.finite(p95), p95 := NA_real_]
volumen_diag_producto[!is.finite(p99), p99 := NA_real_]
volumen_diag_producto[!is.finite(max_vol), max_vol := NA_real_]

cat("\nSummary by product:\n")
print(volumen_diag_producto)

# By product x sales channel (canal_de_comercializacion)
volumen_diag_prod_canal <- eess_all[
  , .(
    n = .N,
    n_na_volumen = sum(is.na(volumen_num)),
    share_na_volumen = mean(is.na(volumen_num)),
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    share_cero = mean(volumen_num == 0, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, canal_de_comercializacion)
][order(-n)]

volumen_diag_prod_canal[!is.finite(p50), p50 := NA_real_]
volumen_diag_prod_canal[!is.finite(p95), p95 := NA_real_]
volumen_diag_prod_canal[!is.finite(p99), p99 := NA_real_]
volumen_diag_prod_canal[!is.finite(max_vol), max_vol := NA_real_]

cat("\nSummary by product and channel (30 largest cells):\n")
print(volumen_diag_prod_canal[1:30])

# Do all rows with product "N/D" have zero volume?
eess_all[
  producto == "N/D",
  .(
    n = .N,
    n_cero = sum(volumen_num == 0, na.rm = TRUE),
    n_no_cero = sum(volumen_num != 0, na.rm = TRUE),
    min_vol = min(volumen_num, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  )
]

# And the other way round: which products do the zeros belong to?
eess_all[
  volumen_num == 0,
  .N,
  by = producto
][order(-N)]

# GNC (compressed natural gas) on its own
eess_all[
  producto == "GNC",
  .(
    n = .N,
    p1 = quantile(volumen_num, 0.01, na.rm = TRUE),
    p5 = quantile(volumen_num, 0.05, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  )
]

# 2.4 Outliers by product ----

# Outliers are defined within product, never on the pooled distribution.
# Upper percentiles and median by product (N/D excluded):
vol_cutoffs_prod <- eess_all[
  !is.na(volumen_num) & producto != "N/D",
  .(
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    p9999 = quantile(volumen_num, 0.9999, na.rm = TRUE),
    mediana = median(volumen_num, na.rm = TRUE)
  ),
  by = producto
]

print(vol_cutoffs_prod)

# Attach the cutoffs and flag extreme observations within each product
eess_all <- vol_cutoffs_prod[eess_all, on = "producto"]

eess_all[, flag_outlier_p999 := !is.na(volumen_num) & volumen_num > p999]
eess_all[, flag_outlier_p9999 := !is.na(volumen_num) & volumen_num > p9999]

eess_all[, flag_outlier_ratio := !is.na(volumen_num) & !is.na(mediana) & mediana > 0 &
           volumen_num > 1000 * mediana]

# Stations that concentrate the observations above the 99.99th percentile
outliers_por_estacion <- eess_all[
  flag_outlier_p9999 == TRUE,
  .(
    n_outliers = .N,
    productos = paste(sort(unique(producto)), collapse = " | "),
    min_fecha = min(periodo_dt, na.rm = TRUE),
    max_fecha = max(periodo_dt, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_outliers, -max_vol)]

print(outliers_por_estacion[1:30])

# Full history of two suspicious stations
eess_all[
  nro_inscripcion %in% c(8651, 4153)
][order(nro_inscripcion, producto, periodo_dt),
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion,
    volumen, volumen_num, p999, p9999, mediana,
    flag_outlier_p999, flag_outlier_p9999, flag_outlier_ratio, source_file)
]

# Stricter flag for clearly absurd values: more than 10 times the product's
# 99.99th percentile, plus an absolute cap of 1e9 for GNC
eess_all[, flag_absurdo_prod := !is.na(volumen_num) & !is.na(p9999) & volumen_num > 10 * p9999]

eess_all[, flag_absurdo_gnc := producto == "GNC" & !is.na(volumen_num) & volumen_num > 1e9]

eess_all[, flag_absurdo_volumen := flag_absurdo_prod | flag_absurdo_gnc]

eess_all[, .N, by = .(producto, flag_absurdo_volumen)][order(producto, -flag_absurdo_volumen)]

# Rows flagged as absurd
absurdos <- eess_all[
  flag_absurdo_volumen == TRUE,
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion,
    volumen, volumen_num, mediana, p999, p9999, source_file)
][order(producto, -volumen_num)]

print(absurdos[1:100])

# 2.5 High volumes by product, business type and year ----

stopifnot("producto" %in% names(eess_all))
stopifnot("volumen_num" %in% names(eess_all))

if (!"anio" %in% names(eess_all)) {
  if ("periodo_dt" %in% names(eess_all)) {
    eess_all[, anio := as.integer(format(periodo_dt, "%Y"))]
  } else if ("periodo" %in% names(eess_all)) {
    eess_all[, anio := as.integer(substr(periodo, 1, 4))]
  } else {
    stop("Neither 'anio' nor a period variable to derive it from was found.")
  }
}

# Business-type column: tipo_negocio if present, otherwise the first column name
# that matches the pattern
cand_tipo_negocio <- grep("tipo.*negocio|negocio.*tipo|tipo_negocio", names(eess_all), value = TRUE)

cat("Candidate variables for tipo_negocio:\n")
print(cand_tipo_negocio)

if ("tipo_negocio" %in% names(eess_all)) {
  tipo_negocio_var <- "tipo_negocio"
} else if (length(cand_tipo_negocio) >= 1) {
  tipo_negocio_var <- cand_tipo_negocio[1]
} else {
  stop("No business-type variable found. See 'cand_tipo_negocio'.")
}

cat("Business-type variable used:", tipo_negocio_var, "\n")

# Character version, with missing and blank values as their own category
eess_all[, tipo_negocio_std := as.character(get(tipo_negocio_var))]
eess_all[is.na(tipo_negocio_std) | trimws(tipo_negocio_std) == "", tipo_negocio_std := "MISSING"]

cat("\nMost frequent values of tipo_negocio:\n")
print(eess_all[, .N, by = tipo_negocio_std][order(-N)][1:30])

# Cutoffs by product, so that GNC is not pooled with gasoline and diesel
vol_cutoffs_prod <- eess_all[
  !is.na(volumen_num) & producto != "N/D",
  .(
    n_prod = .N,
    mediana_prod = median(volumen_num, na.rm = TRUE),
    p99_prod = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999_prod = quantile(volumen_num, 0.999, na.rm = TRUE),
    p9999_prod = quantile(volumen_num, 0.9999, na.rm = TRUE),
    max_prod = max(volumen_num, na.rm = TRUE)
  ),
  by = producto
]

print(vol_cutoffs_prod)

eess_all <- vol_cutoffs_prod[eess_all, on = "producto"]

# High-volume flags by product
eess_all[, flag_hi_p99 := !is.na(volumen_num) & !is.na(p99_prod) & volumen_num > p99_prod]
eess_all[, flag_hi_p999 := !is.na(volumen_num) & !is.na(p999_prod) & volumen_num > p999_prod]
eess_all[, flag_hi_p9999 := !is.na(volumen_num) & !is.na(p9999_prod) & volumen_num > p9999_prod]

# Absurd: more than 10 times the 99.99th percentile of the product, or more
# than 1,000 times its median
eess_all[, flag_absurdo_10xp9999 := !is.na(volumen_num) & !is.na(p9999_prod) & volumen_num > 10 * p9999_prod]

eess_all[, flag_absurdo_ratio := !is.na(volumen_num) & !is.na(mediana_prod) & mediana_prod > 0 &
           volumen_num > 1000 * mediana_prod]

eess_all[, flag_absurdo_vol := flag_absurdo_10xp9999 | flag_absurdo_ratio]

# Are high volumes concentrated among wholesalers? By product x business type
res_prod_tipo <- eess_all[
  producto != "N/D" & !is.na(volumen_num),
  .(
    n = .N,
    n_hi_p999 = sum(flag_hi_p999, na.rm = TRUE),
    share_hi_p999 = mean(flag_hi_p999, na.rm = TRUE),
    n_hi_p9999 = sum(flag_hi_p9999, na.rm = TRUE),
    share_hi_p9999 = mean(flag_hi_p9999, na.rm = TRUE),
    n_absurdo = sum(flag_absurdo_vol, na.rm = TRUE),
    share_absurdo = mean(flag_absurdo_vol, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, tipo_negocio_std)
][order(producto, -n)]

cat("\nSummary by product x tipo_negocio\n")
print(res_prod_tipo)

# In which years do they occur?
res_anio_prod_tipo <- eess_all[
  producto != "N/D" & !is.na(volumen_num),
  .(
    n = .N,
    n_hi_p9999 = sum(flag_hi_p9999, na.rm = TRUE),
    share_hi_p9999 = mean(flag_hi_p9999, na.rm = TRUE),
    n_absurdo = sum(flag_absurdo_vol, na.rm = TRUE),
    share_absurdo = mean(flag_absurdo_vol, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(anio, producto, tipo_negocio_std)
][order(anio, producto, tipo_negocio_std)]

cat("\nSummary by year x product x tipo_negocio\n")
print(res_anio_prod_tipo)

res_anio <- eess_all[
  producto != "N/D" & !is.na(volumen_num),
  .(
    n = .N,
    n_hi_p9999 = sum(flag_hi_p9999, na.rm = TRUE),
    share_hi_p9999 = mean(flag_hi_p9999, na.rm = TRUE),
    n_absurdo = sum(flag_absurdo_vol, na.rm = TRUE),
    share_absurdo = mean(flag_absurdo_vol, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = anio
][order(anio)]

cat("\nSummary by year\n")
print(res_anio)

# Stations with the most extreme values
outliers_por_estacion <- eess_all[
  flag_hi_p9999 == TRUE,
  .(
    n_hi_p9999 = .N,
    n_absurdo = sum(flag_absurdo_vol, na.rm = TRUE),
    productos = paste(sort(unique(producto)), collapse = " | "),
    tipos_negocio = paste(sort(unique(tipo_negocio_std)), collapse = " | "),
    min_anio = min(anio, na.rm = TRUE),
    max_anio = max(anio, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_absurdo, -n_hi_p9999, -max_vol)]

cat("\nStations with the most extreme values\n")
print(outliers_por_estacion[1:50])

# Rows above the 99.99th percentile of their product
extremos_rows <- eess_all[
  flag_hi_p9999 == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_prod, p999_prod, p9999_prod,
    flag_hi_p9999, flag_absurdo_vol, source_file
  )
][order(producto, -volumen_num)]

cat("\nTop 100 extreme rows\n")
print(extremos_rows[1:100])

# GNC only
res_gnc_anio_tipo <- eess_all[
  producto == "GNC" & !is.na(volumen_num),
  .(
    n = .N,
    n_hi_p9999 = sum(flag_hi_p9999, na.rm = TRUE),
    share_hi_p9999 = mean(flag_hi_p9999, na.rm = TRUE),
    n_absurdo = sum(flag_absurdo_vol, na.rm = TRUE),
    share_absurdo = mean(flag_absurdo_vol, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(anio, tipo_negocio_std)
][order(anio, tipo_negocio_std)]

cat("\nGNC by year x tipo_negocio\n")
print(res_gnc_anio_tipo)

# The two suspicious stations again, now with business type and the new flags
estaciones_sospechosas <- c(8651, 4153)

detalle_sospechosas <- eess_all[
  nro_inscripcion %in% estaciones_sospechosas,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_prod, p999_prod, p9999_prod,
    flag_hi_p9999, flag_absurdo_vol, source_file
  )
][order(nro_inscripcion, producto, periodo_dt)]

cat("\nDetail of suspicious stations\n")
print(detalle_sospechosas[1:200])

# 2.6 Outliers within product x business type ----

vol_cutoffs_prod_tipo <- eess_all[
  producto != "N/D" & !is.na(volumen_num),
  .(
    n_grupo = .N,
    mediana_pt = median(volumen_num, na.rm = TRUE),
    p99_pt = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999_pt = quantile(volumen_num, 0.999, na.rm = TRUE),
    p9999_pt = quantile(volumen_num, 0.9999, na.rm = TRUE),
    max_pt = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, tipo_negocio_std)
]

eess_all <- vol_cutoffs_prod_tipo[eess_all, on = .(producto, tipo_negocio_std)]

# Same flags as before, now relative to the product x business type group
eess_all[, flag_hi_p999_pt := !is.na(volumen_num) & !is.na(p999_pt) & volumen_num > p999_pt]
eess_all[, flag_hi_p9999_pt := !is.na(volumen_num) & !is.na(p9999_pt) & volumen_num > p9999_pt]

eess_all[, flag_absurdo_ratio_pt := !is.na(volumen_num) & !is.na(mediana_pt) & mediana_pt > 0 &
           volumen_num > 1000 * mediana_pt]

eess_all[, flag_absurdo_10xp9999_pt := !is.na(volumen_num) & !is.na(p9999_pt) &
           volumen_num > 10 * p9999_pt]

# Conservative final flag: either rule
eess_all[, flag_absurdo_pt := flag_absurdo_ratio_pt | flag_absurdo_10xp9999_pt]

res_prod_tipo_nuevo <- eess_all[
  producto != "N/D" & !is.na(volumen_num),
  .(
    n = .N,
    n_hi_p9999_pt = sum(flag_hi_p9999_pt, na.rm = TRUE),
    share_hi_p9999_pt = mean(flag_hi_p9999_pt, na.rm = TRUE),
    n_absurdo_pt = sum(flag_absurdo_pt, na.rm = TRUE),
    share_absurdo_pt = mean(flag_absurdo_pt, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, tipo_negocio_std)
][order(producto, -n)]

print(res_prod_tipo_nuevo)

# Rows that are still extreme under the new cutoffs
extremos_pt <- eess_all[
  flag_hi_p9999_pt == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt,
    flag_hi_p9999_pt, flag_absurdo_pt, source_file
  )
][order(producto, tipo_negocio_std, -volumen_num)]

print(extremos_pt[1:100])

# Stations that still show up as extreme
extremos_pt_estacion <- eess_all[
  flag_hi_p9999_pt == TRUE,
  .(
    n_hi_p9999_pt = .N,
    n_absurdo_pt = sum(flag_absurdo_pt, na.rm = TRUE),
    productos = paste(sort(unique(producto)), collapse = " | "),
    tipos_negocio = paste(sort(unique(tipo_negocio_std)), collapse = " | "),
    min_anio = min(anio, na.rm = TRUE),
    max_anio = max(anio, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_absurdo_pt, -n_hi_p9999_pt, -max_vol)]

print(extremos_pt_estacion[1:50])

# First cleaned version: every value flagged as absurd becomes NA. It is
# rebuilt below with narrower rules.
eess_all[, volumen_num_limpio := volumen_num]
eess_all[flag_absurdo_pt == TRUE, volumen_num_limpio := NA_real_]

# Values removed, by product
eess_all[
  , .(
    n_total = .N,
    n_na_original = sum(is.na(volumen_num)),
    n_na_limpio = sum(is.na(volumen_num_limpio)),
    n_eliminados = sum(is.na(volumen_num_limpio) & !is.na(volumen_num))
  ),
  by = producto
][order(-n_eliminados)]

# 2.7 GNC: implausibly large volumes ----

# Option A: every GNC value flagged as absurd becomes NA
eess_all[, flag_gnc_absurda := producto == "GNC" & flag_absurdo_pt == TRUE]

eess_all[, volumen_num_limpio_A := volumen_num]
eess_all[flag_gnc_absurda == TRUE, volumen_num_limpio_A := NA_real_]

eess_all[
  , .(
    n_total = .N,
    n_na_original = sum(is.na(volumen_num)),
    n_na_limpio_A = sum(is.na(volumen_num_limpio_A)),
    n_eliminados_A = sum(is.na(volumen_num_limpio_A) & !is.na(volumen_num))
  ),
  by = producto
][order(-n_eliminados_A)]

# Rows that option A would remove, and how many per station
gnc_absurdas_rows <- eess_all[
  flag_gnc_absurda == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt,
    flag_absurdo_pt, source_file
  )
][order(nro_inscripcion, periodo_dt)]

print(gnc_absurdas_rows[1:200])

gnc_absurdas_estacion <- eess_all[
  flag_gnc_absurda == TRUE,
  .(
    n = .N,
    min_fecha = min(periodo_dt),
    max_fecha = max(periodo_dt),
    max_vol = max(volumen_num)
  ),
  by = nro_inscripcion
][order(-n, -max_vol)]

print(gnc_absurdas_estacion)

# Option A2: only truly huge GNC values. The second condition is a subset of the
# first, so the rule amounts to GNC volumes of 1e9 or more.
eess_all[, flag_gnc_monstruosa := producto == "GNC" & (
  volumen_num >= 1e9 |
    (flag_absurdo_pt == TRUE & volumen_num >= 1e10)
)]

eess_all[, volumen_num_limpio_A2 := volumen_num]
eess_all[flag_gnc_monstruosa == TRUE, volumen_num_limpio_A2 := NA_real_]

eess_all[
  , .(
    n_total = .N,
    n_na_original = sum(is.na(volumen_num)),
    n_na_limpio_A2 = sum(is.na(volumen_num_limpio_A2)),
    n_eliminados_A2 = sum(is.na(volumen_num_limpio_A2) & !is.na(volumen_num))
  ),
  by = producto
][order(-n_eliminados_A2)]

gnc_monstruosa_estacion <- eess_all[
  flag_gnc_monstruosa == TRUE,
  .(
    n = .N,
    min_fecha = min(periodo_dt),
    max_fecha = max(periodo_dt),
    max_vol = max(volumen_num)
  ),
  by = nro_inscripcion
][order(-n, -max_vol)]

print(gnc_monstruosa_estacion)

gnc_monstruosa_rows <- eess_all[
  flag_gnc_monstruosa == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt,
    flag_absurdo_pt, source_file
  )
][order(nro_inscripcion, periodo_dt)]

print(gnc_monstruosa_rows[1:200])

# Option A2 is the one kept: volumen_num_limpio is rebuilt with it
eess_all[, flag_gnc_monstruosa := producto == "GNC" & (
  volumen_num >= 1e9 |
    (flag_absurdo_pt == TRUE & volumen_num >= 1e10)
)]

eess_all[, volumen_num_limpio := volumen_num]
eess_all[flag_gnc_monstruosa == TRUE, volumen_num_limpio := NA_real_]

# 2.8 Liquid fuels: implausibly large volumes ----

# Inspection first; nothing is removed in this block
liquidos <- c(
  "Gas Oil Grado 1",
  "Gas Oil Grado 2",
  "Gas Oil Grado 2B",
  "Gas Oil Grado 3",
  "Nafta (común) hasta 92 Ron",
  "Nafta (súper) entre 92 y 95 Ron",
  "Nafta (premium) de más de 95 Ron",
  "Kerosene",
  "Biodiesel",
  "GLPA"
)

liquidos_sospechosos <- eess_all[
  producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt, source_file
  )
][order(producto, tipo_negocio_std, canal_de_comercializacion, -volumen_num)]

print(liquidos_sospechosos[1:200])

res_liquidos_tipo <- eess_all[
  producto %in% liquidos,
  .(
    n = .N,
    n_absurdo = sum(flag_absurdo_pt, na.rm = TRUE),
    share_absurdo = mean(flag_absurdo_pt, na.rm = TRUE),
    p50 = median(volumen_num, na.rm = TRUE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE),
    p999 = quantile(volumen_num, 0.999, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(producto, tipo_negocio_std)
][order(producto, -n_absurdo, -max_vol)]

print(res_liquidos_tipo)

res_liquidos_anio <- eess_all[
  producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    n_absurdo = .N,
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(anio, producto, tipo_negocio_std, canal_de_comercializacion)
][order(anio, producto, tipo_negocio_std, canal_de_comercializacion)]

print(res_liquidos_anio)

liq_sospechosas_estacion <- eess_all[
  producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    n_absurdos = .N,
    productos = paste(sort(unique(producto)), collapse = " | "),
    tipos = paste(sort(unique(tipo_negocio_std)), collapse = " | "),
    canales = paste(sort(unique(canal_de_comercializacion)), collapse = " | "),
    min_anio = min(anio, na.rm = TRUE),
    max_anio = max(anio, na.rm = TRUE),
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = nro_inscripcion
][order(-n_absurdos, -max_vol)]

print(liq_sospechosas_estacion[1:50])

# Keep the GNC cleaning. Suspicious liquids get a flag of their own but are not
# removed yet.
eess_all[, volumen_num_limpio := volumen_num]
eess_all[flag_gnc_monstruosa == TRUE, volumen_num_limpio := NA_real_]

eess_all[, flag_liquido_sospechoso := producto %in% liquidos & flag_absurdo_pt == TRUE]

# Flagged rows of ten stations taken from the ranking above
top_liq_est <- c(2656, 7474, 751, 5335, 11075, 10773, 7485, 2762, 9660, 5839)

detalle_top_liq <- eess_all[
  nro_inscripcion %in% top_liq_est & producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt, source_file
  )
][order(nro_inscripcion, producto, periodo_dt)]

print(detalle_top_liq[1:300])

# Option B2: remove only the liquid-fuel volumes that are clearly impossible
liquidos <- c(
  "Gas Oil Grado 1",
  "Gas Oil Grado 2",
  "Gas Oil Grado 2B",
  "Gas Oil Grado 3",
  "Nafta (común) hasta 92 Ron",
  "Nafta (súper) entre 92 y 95 Ron",
  "Nafta (premium) de más de 95 Ron",
  "Kerosene",
  "Biodiesel",
  "GLPA"
)

liq_gas_nafta <- c(
  "Gas Oil Grado 1",
  "Gas Oil Grado 2",
  "Gas Oil Grado 2B",
  "Gas Oil Grado 3",
  "Nafta (común) hasta 92 Ron",
  "Nafta (súper) entre 92 y 95 Ron",
  "Nafta (premium) de más de 95 Ron"
)

liq_otros <- c("Kerosene", "Biodiesel", "GLPA")

# Volume relative to the 99.99th percentile and to the median of the
# product x business type group
eess_all[, ratio_p9999_pt := fifelse(
  !is.na(p9999_pt) & p9999_pt > 0,
  volumen_num / p9999_pt,
  NA_real_
)]

eess_all[, ratio_mediana_pt := fifelse(
  !is.na(mediana_pt) & mediana_pt > 0,
  volumen_num / mediana_pt,
  NA_real_
)]

# Retail sales to the public
eess_all[, flag_minorista_publico :=
           grepl("venta por menor", tipo_negocio_std, ignore.case = TRUE) &
           canal_de_comercializacion == "Al público"
]

# Business types and channels that can legitimately move large volumes
# (distributors, resellers, agro) are left out
eess_all[, flag_no_mayorista := !grepl(
  "Distribuidor|Revendedor general|Agro|Reventa a otras estaciones",
  paste(tipo_negocio_std, canal_de_comercializacion),
  ignore.case = TRUE
)]

# Absolute floor by product family: 1e6 for diesel and gasoline, 1e5 for
# kerosene, biodiesel and GLPA
eess_all[, piso_abs_liq := fifelse(
  producto %in% liq_gas_nafta, 1e6,
  fifelse(producto %in% liq_otros, 1e5, NA_real_)
)]

# A liquid-fuel volume is flagged only if it is a retail sale to the public by
# a non-wholesale business, exceeds the floor, and is either more than 10 times
# the group's 99.99th percentile or more than 5,000 times its median
eess_all[, flag_liquido_monstruoso :=
           producto %in% liquidos &
           flag_minorista_publico == TRUE &
           flag_no_mayorista == TRUE &
           !is.na(volumen_num) &
           !is.na(piso_abs_liq) &
           volumen_num > piso_abs_liq &
           (
             (!is.na(ratio_p9999_pt) & ratio_p9999_pt > 10) |
               (!is.na(ratio_mediana_pt) & ratio_mediana_pt > 5000)
           )
]

# Flagged rows, and how many per station
liq_monstruosos_rows <- eess_all[
  flag_liquido_monstruoso == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p9999_pt, ratio_mediana_pt, ratio_p9999_pt, source_file
  )
][order(producto, -volumen_num)]

print(liq_monstruosos_rows[1:200])

liq_monstruosos_estacion <- eess_all[
  flag_liquido_monstruoso == TRUE,
  .(
    n = .N,
    productos = paste(sort(unique(producto)), collapse = " | "),
    min_fecha = min(periodo_dt),
    max_fecha = max(periodo_dt),
    max_vol = max(volumen_num)
  ),
  by = nro_inscripcion
][order(-n, -max_vol)]

print(liq_monstruosos_estacion[1:50])

# Final numeric volume: start from the version with GNC already cleaned
eess_all[, volumen_num_limpio_final := volumen_num_limpio]
eess_all[flag_liquido_monstruoso == TRUE, volumen_num_limpio_final := NA_real_]

# Missing values by product at each stage
eess_all[
  , .(
    n_total = .N,
    n_na_original = sum(is.na(volumen_num)),
    n_na_post_gnc = sum(is.na(volumen_num_limpio)),
    n_na_final = sum(is.na(volumen_num_limpio_final)),
    n_extra_liquidos = sum(is.na(volumen_num_limpio_final) & !is.na(volumen_num_limpio))
  ),
  by = producto
][order(-n_extra_liquidos)]

# 2.9 Save cleaned1 ----

# First pass: the panel with every auxiliary column built above
eess_all_cleaned1 <- copy(eess_all)

saveRDS(
  eess_all_cleaned1,
  fs::path(DIR_DATASETS, "eess_all_cleaned1.rds"),
  compress = "gzip"
)

message("Saved to: ", fs::path(DIR_DATASETS, "eess_all_cleaned1.rds"))

eess_all_cleaned1 <- readRDS(fs::path(DIR_DATASETS, "eess_all_cleaned1.rds"))

# The rest of the script only uses files on disk, so it can be resumed from here
# (after sourcing the config)

# The file just written still carries the auxiliary columns. That version is
# stored as eess_all_working_with_aux.rds, and eess_all_cleaned1.rds is
# rewritten with the original columns only.
eess_all_aux_from_old_cleaned1 <- readRDS(
  fs::path(DIR_DATASETS, "eess_all_cleaned1.rds")
)

# Original column set
base_original_ref <- readRDS(fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"))
vars_originales <- names(base_original_ref)

# Final flag: implausibly large GNC or liquid-fuel volume (NA counts as FALSE)
eess_all_aux_from_old_cleaned1[, flag_extremo_volumen_final :=
  fifelse(is.na(flag_gnc_monstruosa), FALSE, flag_gnc_monstruosa) |
  fifelse(is.na(flag_liquido_monstruoso), FALSE, flag_liquido_monstruoso)
]

eess_all_working_with_aux <- copy(eess_all_aux_from_old_cleaned1)

saveRDS(
  eess_all_working_with_aux,
  fs::path(DIR_DATASETS, "eess_all_working_with_aux.rds"),
  compress = "gzip"
)

# cleaned1 keeps the original columns only, and flagged volumes become NA in
# the original (character) variable
eess_all_cleaned1 <- copy(eess_all_aux_from_old_cleaned1)[, ..vars_originales]

eess_all_cleaned1[
  eess_all_aux_from_old_cleaned1$flag_extremo_volumen_final == TRUE,
  volumen := NA_character_
]

saveRDS(
  eess_all_cleaned1,
  fs::path(DIR_DATASETS, "eess_all_cleaned1.rds"),
  compress = "gzip"
)

message("Done. Saved:")
message(" - ", fs::path(DIR_DATASETS, "eess_all_working_with_aux.rds"))
message(" - ", fs::path(DIR_DATASETS, "eess_all_cleaned1.rds"))

colnames(eess_all_cleaned1)

# 2.10 Very small volumes ----

# cleaned1 only keeps the original columns, so the numeric volume is parsed again
vol_chr <- trimws(as.character(eess_all_cleaned1$volumen))
eess_all_cleaned1[, volumen_num := suppressWarnings(as.numeric(vol_chr))]

# Positive values that are almost zero: count, examples and count by product
eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .N
]

eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .(periodo, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num)
][1:50]

eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .N,
  by = producto
][order(-N)]

# Distribution of the values in (0, 1] by order of magnitude, overall and by
# product
eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(
    banda = fifelse(volumen_num <= 1e-5, "(0,1e-5]",
            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
            fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
            fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                    "(1e-1,1]")))))
  )
][order(banda)]

eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(
    producto,
    banda = fifelse(volumen_num <= 1e-5, "(0,1e-5]",
            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
            fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
            fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                    "(1e-1,1]")))))
  )
][order(producto, banda)]

eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 1e-5 & volumen_num <= 1e-1,
  .(periodo, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num)
][1:100]

# In which years do the small values occur?

setDT(eess_all_cleaned1)

# Rebuild volumen_num and anio if they are missing
if (!"volumen_num" %in% names(eess_all_cleaned1)) {
  vol_chr <- trimws(as.character(eess_all_cleaned1$volumen))
  eess_all_cleaned1[, volumen_num := suppressWarnings(as.numeric(vol_chr))]
}

if (!"anio" %in% names(eess_all_cleaned1)) {
  eess_all_cleaned1[, anio := as.integer(substr(periodo, 1, 4))]
}

small_by_year <- eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(
    anio,
    banda = fifelse(volumen_num <= 1e-5, "(0,1e-5]",
            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
            fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
            fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                    "(1e-1,1]")))))
  )
][order(anio, banda)]

print(small_by_year)

small_by_year_prod <- eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(
    anio, producto,
    banda = fifelse(volumen_num <= 1e-5, "(0,1e-5]",
            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
            fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
            fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                    "(1e-1,1]")))))
  )
][order(anio, producto, banda)]

print(small_by_year_prod)

small_by_year_canal <- eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1,
  .N,
  by = .(
    anio, canal_de_comercializacion,
    banda = fifelse(volumen_num <= 1e-5, "(0,1e-5]",
            fifelse(volumen_num <= 1e-4, "(1e-5,1e-4]",
            fifelse(volumen_num <= 1e-3, "(1e-4,1e-3]",
            fifelse(volumen_num <= 1e-2, "(1e-3,1e-2]",
            fifelse(volumen_num <= 1e-1, "(1e-2,1e-1]",
                    "(1e-1,1]")))))
  )
][order(anio, canal_de_comercializacion, banda)]

print(small_by_year_canal)

# Microscopic values only (<= 1e-5): counts and examples
tiny_by_year <- eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .N,
  by = .(anio, producto, canal_de_comercializacion)
][order(anio, -N)]

print(tiny_by_year)

tiny_examples <- eess_all_cleaned1[
  !is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5,
  .(periodo, anio, nro_inscripcion, producto, canal_de_comercializacion, volumen, volumen_num)
][order(anio, producto, canal_de_comercializacion, volumen_num)]

print(tiny_examples[1:100])

# Final numeric volume: values in (0, 1e-5] are recoded to 0
eess_all_cleaned1[, volumen_num_final := volumen_num]
eess_all_cleaned1[
  !is.na(volumen_num_final) & volumen_num_final > 0 & volumen_num_final <= 1e-5,
  volumen_num_final := 0
]

eess_all_cleaned1[
  , .(
    n_tiny_original = sum(!is.na(volumen_num) & volumen_num > 0 & volumen_num <= 1e-5),
    n_tiny_final = sum(!is.na(volumen_num_final) & volumen_num_final > 0 & volumen_num_final <= 1e-5),
    n_ceros_final = sum(volumen_num_final == 0, na.rm = TRUE)
  )
]

# 2.11 Save cleaned2, cleaned2_cut and cleaned3_cut ----

# cleaned2:     cleaned1 plus volumen_num and volumen_num_final
# cleaned2_cut: cleaned2 without the 229 rows where volumen_num_final is NA
# cleaned3_cut: cleaned2_cut restricted to volumen_num_final >= 1e-3, which drops
#               57,157 rows (zeros included); volumen is overwritten with the
#               final numeric value and the two auxiliary columns are removed

setDT(eess_all_cleaned1)

saveRDS(eess_all_cleaned1,
        fs::path(DIR_DATASETS, "eess_all_cleaned2.rds"), compress = "gzip")

eess_all_cleaned2_cut <- eess_all_cleaned1[!is.na(volumen_num_final)]
saveRDS(eess_all_cleaned2_cut,
        fs::path(DIR_DATASETS, "eess_all_cleaned2_cut.rds"), compress = "gzip")

eess_all_cleaned3_cut <- eess_all_cleaned2_cut[volumen_num_final >= 1e-3]
eess_all_cleaned3_cut[, volumen := volumen_num_final]
eess_all_cleaned3_cut[, c("volumen_num", "volumen_num_final") := NULL]
saveRDS(eess_all_cleaned3_cut,
        fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds"), compress = "gzip")

message("cleaned2, cleaned2_cut and cleaned3_cut saved.")
message("  rows: cleaned2_cut = ", nrow(eess_all_cleaned2_cut),
        " | cleaned3_cut = ", nrow(eess_all_cleaned3_cut))

# Part 3. Prices ----

# Flags extreme values of precio_sin_impuestos (price net of taxes) and drops
# them: zeros, positive prices of 0.1 or less and prices above 100,000.
#
# Input:  eess_all_cleaned3_cut.rds
# Output: eess_all_cleaned4_cut.rds

FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds")

if (!fs::file_exists(FILE_BASE)) {
  stop("File not found: ", FILE_BASE)
}

eess_all_cleaned3_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned3_cut)

cat("Data loaded\n")
cat("Rows:", nrow(eess_all_cleaned3_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned3_cut), "\n")

# 3.1 Basic check of the price variable ----

var_precio <- "precio_sin_impuestos"

if (!var_precio %in% names(eess_all_cleaned3_cut)) {
  stop("Variable ", var_precio, " is not in the data.")
}

cat("\nOriginal class of the variable:\n")
print(class(eess_all_cleaned3_cut[[var_precio]]))

cat("\nFirst values of the original variable:\n")
print(head(eess_all_cleaned3_cut[[var_precio]], 20))

# 3.2 Numeric version ----

# The original column is left untouched: a trimmed text copy and a numeric
# version are added as new columns.
eess_all_cleaned3_cut[, precio_sin_impuestos_chr := trimws(as.character(get(var_precio)))]

eess_all_cleaned3_cut[, precio_sin_impuestos_num := suppressWarnings(
  as.numeric(precio_sin_impuestos_chr)
)]

# 3.3 Conversion diagnostics ----

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

# 3.4 Distribution of the numeric price ----

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

# 3.5 Summary by year ----

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

# 3.6 Flags for extreme prices ----

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

# 3.7 Flag counts: overall, by year and by product ----

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

# 3.8 Examples of flagged records ----

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

# 3.9 Frequency table by price band ----

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

# 3.10 Tentative clean variable (v1) ----

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

# 3.11 Final removal flag (v2) and filtered panel ----

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

# 3.12 Save ----

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

# Part 4. Duplicate records ----

# Studies records that share the same key (period, registration number, product
# and sales channel), across source files and within a file, and removes the
# copies whose values are identical.
#
# Input:  eess_all_cleaned4_cut.rds
# Output: eess_all_cleaned5_cut.rds

# 4.1 Diagnostics of repeated keys ----

FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned4_cut.rds")

eess_all_cleaned4_cut <- readRDS(FILE_BASE)
setDT(eess_all_cleaned4_cut)

cat("Data loaded\n")
cat("Rows:", nrow(eess_all_cleaned4_cut), "\n")
cat("Columns:", ncol(eess_all_cleaned4_cut), "\n")

# A record is identified by key_vars. Records with the same key are compared on
# substantive_vars: volume, prices, taxes and the other reported amounts.
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

# 4.1.1 Overlaps between source files ----

# Keys that appear in more than one source_file
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

overlap_pairs <- key_source_summary[
  , .N,
  by = source_files
][order(-N)]

cat("\nOverlaps by combination of files:\n")
print(overlap_pairs)

key_source_summary[, anio_key := as.integer(format(periodo_dt, "%Y"))]

overlap_year <- key_source_summary[
  , .N,
  by = .(anio_key, source_files)
][order(anio_key, -N)]

cat("\nOverlaps by year and combination of files:\n")
print(overlap_year)

# Rows behind those keys
overlap_rows <- eess_all_cleaned4_cut[key_source_summary, on = key_vars][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

cat("\nRows involved in overlaps between files:", nrow(overlap_rows), "\n")

# 4.1.2 Values of the overlapping records ----

# Number of distinct values of each substantive variable within a key (NA
# counts as a value). The copies carry the same values when every variable has
# exactly one.
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

# 4.1.3 Repeated keys within the same source file ----

key_source_dup <- key_source[n_obs > 1]

cat("\nKeys repeated within the same source_file:", nrow(key_source_dup), "\n")

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

# 4.1.4 Summary of the diagnostics ----

resumen_dup_v3 <- data.table(
  filas_originales = nrow(eess_all_cleaned4_cut),
  keys_en_mas_de_un_source = nrow(key_source_summary),
  filas_solapadas_entre_archivos = nrow(overlap_rows),
  keys_repetidas_dentro_mismo_source = nrow(key_source_dup),
  filas_repetidas_dentro_mismo_source = nrow(within_rows)
)

cat("\nOverall summary:\n")
print(resumen_dup_v3)

# 4.2 Deduplication ----

# The panel is read again from disk and the comparisons of part 1 are rebuilt,
# this time keeping the keys that are safe to deduplicate: those whose copies
# carry the same values. Keys with different values are left as they are.

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

stopifnot(all(key_vars %in% names(eess_all_cleaned4_cut)))
stopifnot(all(substantive_vars %in% names(eess_all_cleaned4_cut)))
stopifnot("source_file" %in% names(eess_all_cleaned4_cut))

# 4.2.1 Overlaps between source files ----

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

# 4.2.2 Repeated keys within the same source file ----

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

# 4.2.3 Step 1: copies between files ----

# Rows to drop are identified by a row id instead of fsetdiff()
eess_all_cleaned4_cut[, row_id := .I]

overlap_safe_rows <- eess_all_cleaned4_cut[overlap_safe_keys, on = key_vars][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file, row_id)
]

# Within each safe key the first row is kept and the rest are dropped. Rows are
# sorted by source_file and row_id, so the row kept comes from the file whose
# name sorts first.
overlap_rows_to_drop <- overlap_safe_rows[
  , .SD[-1],
  by = key_vars
]

cat("\nRows to drop, safe overlap between files:", nrow(overlap_rows_to_drop), "\n")

base_step1 <- eess_all_cleaned4_cut[!row_id %in% overlap_rows_to_drop$row_id]

cat("Rows after step 1:", nrow(base_step1), "\n")

# 4.2.4 Step 2: copies within the same file ----

# The within-file comparison is redone on the panel left after step 1
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

within_safe_keys_step1 <- within_compare_step1[
  all_equal_substantive == TRUE,
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file)
]

cat("Keys safe to deduplicate within the same file (after step 1):", nrow(within_safe_keys_step1), "\n")

within_safe_rows_step1 <- base_step1[within_safe_keys_step1, on = c(key_vars, "source_file")][
  order(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion, source_file, row_id)
]

within_rows_to_drop <- within_safe_rows_step1[
  , .SD[-1],
  by = c(key_vars, "source_file")
]

cat("Rows to drop, safe duplicate within the same file:", nrow(within_rows_to_drop), "\n")

# 4.2.5 Deduplicated panel ----

eess_all_cleaned5_cut <- base_step1[!row_id %in% within_rows_to_drop$row_id]

eess_all_cleaned5_cut[, row_id := NULL]

cat("\nFinal rows in cleaned5:", nrow(eess_all_cleaned5_cut), "\n")
cat("Final columns in cleaned5:", ncol(eess_all_cleaned5_cut), "\n")

# 4.2.6 Final checks ----

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

# 4.2.7 Save ----

saveRDS(eess_all_cleaned5_cut, FILE_OUT)

cat("\nData saved to:\n")
cat(FILE_OUT, "\n")

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

# Part 5. Type of outlet ----

# Replaces the generic label "Estación de servicio" in tipo_negocio (business
# type) with a type inferred from the products each boca (outlet) sells.
#
# Input:  eess_all_cleaned5_cut.rds
# Output: eess_all_cleaned6_cut_nostations.rds (type inferred month by month)
#         eess_all_cleaned7_alternative_sinceappearance.rds (final panel)

FILE_BASE <- fs::path(DIR_DATASETS, "eess_all_cleaned5_cut.rds")

# Parts 1 to 4 diagnose tipo_negocio, infer the type month by month, save that
# panel and check how stable the monthly type is within a station. Parts 5 and 6
# build station-level alternatives (modal type, "ever", "since first
# appearance"), each starting again from the panel on disk. Part 7 saves the
# final panel, which uses the "since first appearance" version.
# A boca-month is one outlet (nro_inscripcion) in one month (periodo_dt).

# 5.1 Diagnosis of tipo_negocio ----

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

# 5.2 Type inferred month by month ----

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

# 5.3 Save cleaned6: panel without the generic label ----

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

# 5.4 Stability of tipo_negocio_h over time ----

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

# 5.5 Alternative: type fixed per station (mode over time) ----

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

# 5.6 Alternatives: "ever" and "since first appearance" ----

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

# 5.7 Save cleaned7: tipo_negocio = since-appearance version ----

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

# Part 6. Markets: locality to department ----

# Build (or load and validate) the locality -> department crosswalk and apply it
# to the station panel, which leaves the final panel with a `departamento` column.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
#         crosswalk_localidad_departamento.csv  (frozen crosswalk, read by default)
#         crosswalk_correcciones_auditoria.csv  (reviewed corrections; rebuild only)
#         georef_departamentos_ref.csv          (official list of departments)
# Output: eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
#         crosswalk_localidad_departamento.csv  (rewritten only when rebuilding)
#
# Why departments rather than localities. A market in the demand model is a
# province x department x month. The locality is too fine: almost half of the
# localities are served by a single brand, which leaves substitution
# unidentified. The department pools neighboring localities, removes these
# spurious monopolies and keeps the periphery in the sample. In 2024 (channel
# "Al público", one bandera (brand) per station, unbranded stations counted as
# individual firms):
#   stations in a monopoly market:  11.45% by locality -> 2.02% by department
#                                   (core 9.83 -> 0.35, periphery 15.17 -> 5.84)
#   monopoly markets:               43.87% by locality -> 17.91% by department
#
# The crosswalk covers 1,398 (province, locality) pairs. Each pair was checked
# against georef by province id, the residual was assigned from the station
# addresses and external sources, and the resulting 60 corrections are listed,
# with the reason for each one, in crosswalk_correcciones_auditoria.csv
# (crosswalk_revisados_auditoria.csv holds the cases reviewed and left unchanged).
#
# crosswalk_localidad_departamento.csv, with a `fuente` column that records where
# each assignment comes from, is kept frozen as reviewed reference data: by
# default the script reads it, validates it and applies it. Setting
# REBUILD_FROM_API to TRUE rebuilds it from the georef API (needs internet
# access) with the same corrections and checks. georef can change between runs,
# so if the two versions differ the frozen file prevails.
#
# 95 rows of the panel (locality "N/D", a single station) are left with a missing
# department on purpose: "N/D" is a placeholder for missing data. Exclude them
# from the model.

REBUILD_FROM_API <- FALSE   # TRUE rebuilds the crosswalk from georef (about 5 minutes)

# Paths ----
BASE_IN  <- file.path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance.rds")
XW_OUT   <- file.path(DIR_INT, "crosswalk_localidad_departamento.csv")
BASE_OUT <- file.path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
XW_CORR  <- file.path(DIR_INT, "crosswalk_correcciones_auditoria.csv")   # sep = "|"
REF_DEP  <- file.path(DIR_INT, "georef_departamentos_ref.csv")           # official list of departments

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Normalization helpers ----
strip_acc <- function(x) chartr("áéíóúüñÁÉÍÓÚÜÑàèìòù", "aeiouunAEIOUUNaeiou", x)
nk        <- function(x){ x <- toupper(strip_acc(trimws(x))); gsub("\\s+", " ", x) }

# Expand the abbreviations found in locality names before querying georef
normx <- function(s){
  s <- toupper(trimws(s))
  reps <- c("CNEL\\."="CORONEL","GRAL\\."="GENERAL","ING\\."="INGENIERO","VTE\\."="VICENTE",
            "GDOR\\."="GOBERNADOR","GOB\\."="GOBERNADOR","CAP\\."="CAPITAN","PTE\\."="PRESIDENTE",
            "PCIA\\. DE LA PLAZA"="PRESIDENCIA DE LA PLAZA","PCIA\\."="PRESIDENCIA",
            "DR\\."="DOCTOR","ALTE\\."="ALMIRANTE","SGO\\."="SANTIAGO","PTO\\."="PUERTO",
            "S\\.F\\.V\\. DE"="SAN FERNANDO DEL VALLE DE","S\\.A\\. DE"="SAN ANTONIO DE",
            "S\\.M\\. DE"="SAN MIGUEL DE","L\\.N\\."="LEANDRO N","FCO\\.?"="FRANCISCO",
            "J\\. ?B\\."="JUAN B","R\\. DE ESCALADA"="REMEDIOS DE ESCALADA",
            "R\\. SOURDEAUX"="INGENIERO ADOLFO SOURDEAUX","H\\. ASCASUBI"="HILARIO ASCASUBI",
            "G\\. LAFERRERE"="GREGORIO DE LAFERRERE","GREGORIO DE LA FERRERE"="GREGORIO DE LAFERRERE",
            "EL TALAR DE PACHECO"="EL TALAR","VEINTICINCO DE MAYO"="25 DE MAYO",
            "3 DE FEBRERO"="CASEROS")
  for (k in names(reps)) s <- gsub(k, reps[[k]], s)
  s <- gsub("O' ", "O'", s); gsub("\\s+", " ", trimws(s))
}

# georef is queried by province id. Filtering by province name is ambiguous:
# "BUENOS AIRES" also matches "Ciudad Autónoma de Buenos Aires", which sends
# San Nicolás (province of Buenos Aires) to Comuna 1 of the city.
PID <- c("BUENOS AIRES"="06","CATAMARCA"="10","CHACO"="22","CHUBUT"="26","CORDOBA"="14",
         "CORRIENTES"="18","ENTRE RIOS"="30","FORMOSA"="34","JUJUY"="38","LA PAMPA"="42",
         "LA RIOJA"="46","MENDOZA"="50","MISIONES"="54","NEUQUEN"="58","RIO NEGRO"="62",
         "SALTA"="66","SAN JUAN"="70","SAN LUIS"="74","SANTA CRUZ"="78","SANTA FE"="82",
         "SANTIAGO DEL ESTERO"="86","TIERRA DEL FUEGO"="94","TUCUMAN"="90")

# Validation checks (stop the script if the crosswalk is inconsistent) ----
validate_xw <- function(xw, base_pairs){
  # 1) structure: one row per (province, locality); a missing department is
  #    accepted only under fuente "sin_dato"
  stopifnot(nrow(xw) == uniqueN(xw[, .(provincia, localidad)]))
  n_na <- xw[is.na(departamento) & fuente != "sin_dato", .N]
  if (n_na > 0) stop("Check failed: ", n_na, " departments are NA outside fuente='sin_dato'")
  # 2) coverage: every pair in the panel has a row in the crosswalk
  anti <- base_pairs[!unique(xw[, .(provincia, loc = trimws(localidad))]),
                     on = c("provincia","loc")]
  if (nrow(anti) > 0) { print(anti); stop("Check failed: pairs in the panel missing from the crosswalk") }
  # 3) a single label per department, so that spelling variants do not split a
  #    market (e.g. 'Fontana')
  lbl <- xw[!is.na(departamento),
            .(n = uniqueN(departamento)), by = .(provincia, dep_k = nk(departamento))][n > 1]
  if (nrow(lbl) > 0) { print(lbl); stop("Check failed: same department under different labels") }
  # 4) each department belongs to its province according to the official list
  #    (local file, no API call)
  if (file.exists(REF_DEP)) {
    ref <- fread(REF_DEP, encoding = "UTF-8")
    pm  <- c("CAPITAL FEDERAL"="CIUDAD AUTONOMA DE BUENOS AIRES",
             "TIERRA DEL FUEGO"="TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR")
    off <- unique(rbind(ref[, .(prov_k = nk(provincia), dep_k = nk(departamento))],
                        data.table(prov_k = "CIUDAD AUTONOMA DE BUENOS AIRES", dep_k = "CABA")))
    chk <- xw[!is.na(departamento) & !fuente %in% c("paraje","sin_dato")]
    chk[, prov_k := nk(provincia)][provincia %in% names(pm), prov_k := pm[provincia]]
    chk[, dep_k := nk(departamento)]
    bad <- chk[!off, on = c("prov_k","dep_k")]
    if (nrow(bad) > 0) { print(bad[, .(provincia, localidad, departamento, fuente)])
                         stop("Check failed: department outside its province") }
    message(sprintf("  checks passed: %d pairs validated against the official list (%d paraje, %d sin_dato exempt)",
                    nrow(chk), xw[fuente=="paraje",.N], xw[fuente=="sin_dato",.N]))
  } else {
    message("  partial checks: ", basename(REF_DEP), " not found (province membership check skipped)")
  }
  invisible(TRUE)
}

# Station panel ----
b <- readRDS(BASE_IN); setDT(b)
base_pairs <- unique(b[, .(provincia, loc = trimws(localidad))])

# 6.1 Get the crosswalk: read the frozen file, or rebuild it and correct it ----
if (!REBUILD_FROM_API && file.exists(XW_OUT)) {

  message("Loading frozen crosswalk: ", basename(XW_OUT))
  xw <- fread(XW_OUT, encoding = "UTF-8")
  if (!"fuente" %in% names(xw)) xw[, fuente := "georef"]   # file saved without the fuente column
  xw <- xw[, .(provincia, localidad, departamento = as.character(departamento), fuente)]
  xw[trimws(departamento) == "" | departamento == "NA", departamento := NA]

} else {

  message("Rebuilding crosswalk from the georef API (one GET per pair, by province id) ...")
  pairs <- unique(b[, .(provincia, localidad = trimws(localidad))])[order(provincia, localidad)]
  message(sprintf("  province-locality pairs: %d", nrow(pairs)))

  # GET with up to 3 attempts and increasing waits. A municipality is not a
  # department, so /municipios is never queried. The department is read from
  # departamento_nombre, except in /departamentos, where it is nombre. With
  # max = 1 georef returns its best fuzzy match, which can be another locality
  # (La Niña -> Arrecifes); those cases are handled in the corrections file.
  GET_dep <- function(path, nombre, provid){
    for (a in 1:3){
      resp <- tryCatch(GET(sprintf("https://apis.datos.gob.ar/georef/api/%s", path),
                           query = list(nombre = nombre, provincia = provid,
                                        max = 1, aplanar = "true"),
                           timeout(60)), error = function(e) NULL)
      if (!is.null(resp) && status_code(resp) == 200){
        r <- fromJSON(content(resp, "text", encoding = "UTF-8"))[[gsub("-","_",path)]]
        if (is.null(r) || length(r) == 0) return(NA_character_)
        col <- if (path == "departamentos") "nombre" else "departamento_nombre"
        if (!col %in% names(as.data.table(r))) return(NA_character_)
        return(as.character(as.data.table(r)[[col]][1]))
      }
      Sys.sleep(1.5 * a)
    }
    stop("no response from georef after 3 attempts: ", path, " / ", nombre)  # fail rather than return a silent NA
  }

  pairs[, `:=`(departamento = NA_character_, fuente = NA_character_)]
  pairs[provincia == "CAPITAL FEDERAL", `:=`(departamento = "CABA", fuente = "caba")]

  cascada <- c("localidades-censales","localidades","asentamientos","departamentos")
  for (i in which(is.na(pairs$departamento))) {
    provid <- PID[[pairs$provincia[i]]]
    for (path in cascada) {
      for (nom in unique(c(normx(pairs$localidad[i]), pairs$localidad[i]))) {
        got <- GET_dep(path, nom, provid)
        if (!is.na(got)) { pairs[i, `:=`(departamento = got,
                                         fuente = paste0("georef_", path))]; break }
      }
      if (!is.na(pairs$departamento[i])) break
    }
    if (i %% 100 == 0) message(sprintf("  ... %d/%d", i, nrow(pairs)))
  }
  message(sprintf("  unmatched after georef: %d", pairs[is.na(departamento), .N]))

  # Manual dictionary for the residual (mostly Greater Buenos Aires and Córdoba).
  # 'Mayor Luis J. Fontana' is the official spelling of that department.
  manual <- fread(sep = "|", text =
'provincia|localidad|departamento
BUENOS AIRES|ING. MASCHWITZ|Escobar
BUENOS AIRES|GREGORIO DE LA FERRERE|La Matanza
BUENOS AIRES|G. LAFERRERE|La Matanza
BUENOS AIRES|VILLA CELINA|La Matanza
BUENOS AIRES|VILLA INSUPERABLE|La Matanza
BUENOS AIRES|SAN FCO.SOLANO|Quilmes
BUENOS AIRES|SAN FCO. SOLANO|Quilmes
BUENOS AIRES|R. DE ESCALADA|Lanús
BUENOS AIRES|ING. WHITE|Bahía Blanca
BUENOS AIRES|GRAL. VIAMONTE|General Viamonte
BUENOS AIRES|VILLARINO|Villarino
BUENOS AIRES|BONIFACIO|Guaminí
BUENOS AIRES|VILLA MAZA|Adolfo Alsina
BUENOS AIRES|INGENIERO BUDGE|Lomas de Zamora
BUENOS AIRES|ISLA SAN FERNANDO|San Fernando
BUENOS AIRES|BANCALARI|Tigre
BUENOS AIRES|VILLA ALBERTINA|Lomas de Zamora
BUENOS AIRES|VILLA MOQUEHUA|Chivilcoy
BUENOS AIRES|AGUSTIN FERRARI|Merlo
BUENOS AIRES|PARAJE LA BALLENERA|General Pueyrredón
CORDOBA|MONTE CRISTO|Río Primero
CORDOBA|ARGUELLO|Capital
CORDOBA|W. ESCALANTE|Unión
CORDOBA|DALMACIO VELEZ SARSFIELD|Tercero Arriba
CORDOBA|VILLA ANIZACATE|Santa María
CORDOBA|VILLA MARIA DEL RIO SECO|Río Seco
CATAMARCA|VALLE VIEJO|Valle Viejo
CATAMARCA|SAN ANTONIO DE LA PAZ|El Alto
CHACO|CNEL. DUGRATY|Mayor Luis J. Fontana
CHACO|PCIA. DE LA PLAZA|Presidencia de la Plaza
CHACO|AVIATERAI|Independencia
CHUBUT|COLONIA SARMIENTO|Sarmiento
CHUBUT|CERRO DRAGON|Escalante
ENTRE RIOS|COLONIA YERUA|Concordia')
  pairs[manual, on = c("provincia","localidad"),
        `:=`(departamento = i.departamento, fuente = "manual")]

  # Reviewed corrections are applied last so that they override every other
  # source; the reason for each one is given in the file itself.
  if (!file.exists(XW_CORR)) stop("missing ", basename(XW_CORR), ": do not rebuild without the corrections")
  corr <- fread(XW_CORR, sep = "|", encoding = "UTF-8")
  pairs[corr, on = c("provincia","localidad"),
        `:=`(departamento = i.dep_nuevo, fuente = "correccion")]
  pairs[departamento == "__NA__", `:=`(departamento = NA_character_, fuente = "sin_dato")]

  # An isolated paraje (hamlet) that remains unresolved is its own market
  # (recorded as fuente 'paraje')
  pairs[is.na(departamento) & fuente != "sin_dato" | is.na(fuente),
        `:=`(departamento = localidad, fuente = "paraje")]

  xw <- pairs[, .(provincia, localidad, departamento, fuente)]
  message(sprintf("  coverage: %d pairs | %d province-department markets | sources: %s",
                  nrow(xw), uniqueN(xw[!is.na(departamento), .(provincia, departamento)]),
                  paste(xw[, .N, by=fuente][order(-N)][, sprintf("%s=%d", fuente, N)], collapse=", ")))
  validate_xw(xw, base_pairs)
  fwrite(xw, XW_OUT, bom = TRUE)
  message("  crosswalk saved: ", basename(XW_OUT))
}

message("Validating crosswalk ...")
validate_xw(xw, base_pairs)

# 6.2 Apply the crosswalk to the panel and save ----
n0 <- nrow(b)
# Merge on the trimmed locality on both sides (some localities in the panel have
# trailing spaces). xwk has a unique key so that the merge cannot duplicate rows.
xwk <- unique(xw[, .(provincia, loc_key = trimws(localidad), departamento)],
              by = c("provincia","loc_key"))
b[, loc_key := trimws(localidad)]
b   <- merge(b, xwk, by = c("provincia","loc_key"), all.x = TRUE)
b[, loc_key := NULL]
stopifnot(nrow(b) == n0)
n_na <- b[is.na(departamento), .N]
message(sprintf("  rows without department: %d  (expected: 95 = locality 'N/D')", n_na))
if (n_na != 95) warning(sprintf("unexpected number of NA rows: %d (expected 95)", n_na))

saveRDS(b, BASE_OUT)
message(sprintf("\nFinal panel with crosswalk saved (%d rows, %d columns):\n  %s",
                nrow(b), ncol(b), BASE_OUT))
message(sprintf("  province x department markets: %d",
                uniqueN(b[!is.na(departamento), .(provincia, departamento)])))


# 01_bind_raw.R
# Binds the six raw retail files (station-level prices and volumes) into one
# panel, records which columns each file has, builds a monthly date and cleans
# the column names.
#
# Input:  public_vi_access_eess_*.rds (six raw files in DIR_RETAIL)
# Output: eess_all_rawbind.rds (plain bind), eess_all_rawbind_2.rds (with date
#         variables and clean column names) and
#         schema_y_variables_rawfiles.xlsx, all in DIR_INTERIM

library(fs)
library(data.table)
library(openxlsx)

source("code/00_config.R")

# Paths ----

DIR_RAW_MIN <- DIR_RETAIL

DIR_DATASETS <- DIR_INTERIM
fs::dir_create(DIR_DATASETS)

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

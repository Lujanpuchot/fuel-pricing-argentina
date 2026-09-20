# 02_clean_volume.R
# Diagnoses the raw volume variable, parses it to numeric, sets implausibly large
# volumes (GNC and liquid fuels) to NA and drops rows with missing or tiny volume.
#
# Input:  eess_all_rawbind_2.rds
# Output: eess_all_cleaned1.rds, eess_all_working_with_aux.rds,
#         eess_all_cleaned2.rds, eess_all_cleaned2_cut.rds,
#         eess_all_cleaned3_cut.rds
#
# Volume is reported in cubic metres, by outlet, product, sales channel and
# month. Every cutoff below is in those units.
#
# Sections 1 to 6 do not change a single value: they describe the raw variable
# and build outlier flags at three levels of aggregation, pooled, by product,
# and by product and business type, to find out where the implausible volumes
# sit. The two rules that are actually applied come next, one for GNC in
# section 7 and one for liquid fuels in section 8, and sections 9 to 11 write
# the files. The evidence behind the cutoffs is in diagnostics_data_quality.R,
# which is not part of the pipeline.

library(data.table)
library(fs)

source("code/00_config.R")

DIR_DATASETS <- DIR_INTERIM

# Panel saved by 01_build_panel.R
eess_all <- readRDS(fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"))
setDT(eess_all)

# 1. Raw volumen: diagnostics ----

# volumen arrives as text. Before parsing it, this part establishes what the
# strings actually look like: how the source spells a missing value ("N/D",
# "ND", "-", an empty string), whether decimal commas or thousands separators
# turn up, and which raw values are most frequent. That is what decides whether
# a custom parser is needed or plain as.numeric() will do.

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

# 2. Parse to numeric ----

# volumen is stored as character, but in a format that as.numeric() reads
# directly (e.g. "21.9920000", "0E-7"), so no custom parser is needed. "0E-7" is
# how the source writes an exact zero, and it parses to 0 rather than to a tiny
# positive number.
eess_all[, volumen_num := suppressWarnings(as.numeric(vol_chr))]

# 3. Diagnostics of the parsed variable ----

# Checks that the conversion lost nothing that matters, and maps the shape of
# the variable before any rule is written. Three questions: which strings became
# NA although they are not one of the usual missing-value codes, where the mass
# of the distribution sits, and how far the two tails reach. The breakdowns by
# month, by product and by product x channel are there to see whether the
# problems concentrate in a period, a fuel or a way of selling it.

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

# Zeros are worth watching: none of the rules in this script touches them, and
# they only leave the panel with the cut in section 11.
cat("\nBasic counts:\n")
cat("Volume = 0: ", eess_all[volumen_num == 0, .N], "\n")
cat("Volume < 0: ", eess_all[volumen_num < 0, .N], "\n")
cat("Volume > 0: ", eess_all[volumen_num > 0, .N], "\n")
cat("Volume NA: ", eess_all[is.na(volumen_num), .N], "\n")

# Both tails, with the station, product, channel and source file attached. The
# columns are what make the rows readable: an absurd value that repeats at one
# station, in one product, or in one source file is a reporting problem rather
# than a stray keystroke. Negative volumes get a print of their own: no flag in
# this script tests for them, and like the zeros they only leave the panel with
# the cut in section 11.
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

# By month, which shows whether missing values and zeros cluster in particular
# periods or source files rather than running evenly through the sample
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

# By product, the breakdown that is economically meaningful. A cubic metre of
# GNC and a cubic metre of gasoline are not comparable quantities of sales, so
# this is the table the cutoffs are built on.
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

# By product x sales channel (canal_de_comercializacion). The channel separates
# retail sales to the public from resale to other stations and from sales to
# distributors, which move volumes of a different order.
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

# Do all rows with product "N/D" have zero volume? "N/D" is a row with no
# product recorded, and every cutoff from section 4 on leaves it out, so what it
# holds has to be checked here or not at all.
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

# GNC (compressed natural gas) on its own, out to the 99.9th percentile. It is
# singled out because it ends up with a cleaning rule of its own in section 7,
# separate from the one the liquid fuels get.
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

# 4. Outliers by product ----

# Outliers are defined within product, never on the pooled distribution: the
# products sit at scales too different for a common cutoff, GNC above all.
#
# None of the flags built in this section is applied to the data. They serve to
# find the stations, products and periods that produce the extreme values; the
# rules that do set volumes to NA are built in sections 7 and 8.
#
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

# The two flags above and this one are diagnostic: they are printed and never
# applied. The ratio is deliberately loose, 1,000 times the group median, so
# that the printouts show candidates rather than only the worst cases. The
# rule that does remove volume, in section 8, asks for 5,000.
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

# Full history of two suspicious stations. Reading the whole series tells apart
# a station that reports one absurd month from one whose level is wrong
# throughout.
eess_all[
  nro_inscripcion %in% c(8651, 4153)
][order(nro_inscripcion, producto, periodo_dt),
  .(periodo_dt, nro_inscripcion, producto, canal_de_comercializacion,
    volumen, volumen_num, p999, p9999, mediana,
    flag_outlier_p999, flag_outlier_p9999, flag_outlier_ratio, source_file)
]

# Stricter flag for clearly absurd values: more than 10 times the product's
# 99.99th percentile, plus an absolute cap of 1e9 for GNC. The first rule is
# relative to the product's own distribution; the second is an absolute one,
# marking any GNC row above 1e9 cubic metres whatever that distribution looks
# like.
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

# 5. High volumes by product, business type and year ----

# The same exercise as section 4, now cut by tipo_negocio (business type) and by
# year. Some outlets are wholesalers or distributors and move large volumes
# legitimately, so before calling a value absurd it is worth knowing whether the
# large volumes belong to them, and whether they are spread over the sample or
# concentrated in a few years.
#
# The product cutoffs are computed again here under new names (p99_prod,
# p999_prod, p9999_prod) and joined onto the panel a second time, so both sets
# of columns end up in it.

library(data.table)

stopifnot("producto" %in% names(eess_all))
stopifnot("volumen_num" %in% names(eess_all))

# anio (year) is derived from whichever period variable the panel carries, so
# the section also runs on a panel that arrives without it
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
# than 1,000 times its median. The two rules catch different things, a value far
# beyond a tail that is already long and a value out of all proportion to a
# typical month, and either one is enough.
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

# The same counts collapsed to one row per year, which is the quickest way to
# see whether the extremes arrive with a particular period of the source
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

# GNC only, by year and business type, since it is the product whose tail
# drives the whole exercise
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

# 6. Outliers within product x business type ----

# The finest grouping the flags use. Cutoffs are recomputed inside each
# product x business type cell, so a distributor's normal month is measured
# against other distributors and not against retail stations. flag_absurdo_pt is
# the working flag from here on: sections 7 and 8 narrow it down before anything
# is set to NA.

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

# Conservative final flag: either rule. Conservative in the sense of flagging
# generously, since nothing is removed on the strength of this flag alone.
eess_all[, flag_absurdo_pt := flag_absurdo_ratio_pt | flag_absurdo_10xp9999_pt]

# How much the finer grouping changes the picture: the same shares of flagged
# rows as in section 5, now computed against the product x business type
# cutoffs. A cell whose share of absurd rows drops sharply was one where the
# product cutoff had been the wrong yardstick.
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

# 7. GNC: implausibly large volumes ----

# GNC carries the worst of the tail, and no percentile rule handles it well, so
# two candidate rules are written out and compared before one is kept. Option A
# removes everything flag_absurdo_pt marks, which is a lot; option A2 keeps only
# an absolute cutoff and removes the values that cannot be a month of sales on
# any reading. A2 is the one applied.

# Option A: every GNC value flagged as absurd becomes NA
eess_all[, flag_gnc_absurda := producto == "GNC" & flag_absurdo_pt == TRUE]

eess_all[, volumen_num_limpio_A := volumen_num]
eess_all[flag_gnc_absurda == TRUE, volumen_num_limpio_A := NA_real_]

# How many values each option costs, by product. n_eliminados_A counts only the
# volumes that were present and become NA, not the ones that were missing to
# begin with.
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
# first, so the rule amounts to GNC volumes of 1e9 cubic metres or more.
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

# The stations and rows option A2 would remove, to be read against the option A
# lists printed above. The gap between the two is what the choice of rule costs
# or saves.
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

# Option A2 is the one kept: volumen_num_limpio is rebuilt with it, discarding
# the wider version of the flag built at the end of section 6
eess_all[, flag_gnc_monstruosa := producto == "GNC" & (
  volumen_num >= 1e9 |
    (flag_absurdo_pt == TRUE & volumen_num >= 1e10)
)]

eess_all[, volumen_num_limpio := volumen_num]
eess_all[flag_gnc_monstruosa == TRUE, volumen_num_limpio := NA_real_]

# 8. Liquid fuels: implausibly large volumes ----

# Gasoline, diesel and the smaller liquid products have a tail of their own. It
# is less extreme than GNC's and harder to separate from genuine large sales,
# so the section looks first, by product, business type, channel, year and
# station, and only then writes a rule.
#
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

# Every liquid-fuel row the product x business type rule marks, sorted so that
# the largest value of each product and group comes first
liquidos_sospechosos <- eess_all[
  producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    periodo_dt, anio, nro_inscripcion, producto, tipo_negocio_std,
    canal_de_comercializacion, volumen, volumen_num,
    mediana_pt, p999_pt, p9999_pt, source_file
  )
][order(producto, tipo_negocio_std, canal_de_comercializacion, -volumen_num)]

print(liquidos_sospechosos[1:200])

# Share of flagged rows by product and business type. A business type in which
# the share is high across the board is one whose volumes are large by nature,
# and a candidate for being left out of the rule rather than cleaned by it.
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

# The same rows spread over year, product, business type and channel. Flagged
# rows bunched in a few years would point at a change in the source rather than
# at the stations.
res_liquidos_anio <- eess_all[
  producto %in% liquidos & flag_absurdo_pt == TRUE,
  .(
    n_absurdo = .N,
    max_vol = max(volumen_num, na.rm = TRUE)
  ),
  by = .(anio, producto, tipo_negocio_std, canal_de_comercializacion)
][order(anio, producto, tipo_negocio_std, canal_de_comercializacion)]

print(res_liquidos_anio)

# Ranking by station. The ten at the top are followed month by month further
# down, which is what the narrower rule is written against.
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

# Option B2: remove only the liquid-fuel volumes that are clearly impossible.
# The rule is a conjunction of the conditions built below, so that it reaches
# only retail sales to the public that are large in absolute terms and out of
# proportion to their own group.
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

# Retail sales to the public: tipo_negocio labels of the form
# "Bocas de expendio (venta por menor) ..." sold through the "Al público"
# channel. Wholesale rows never enter the rule.
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

# Absolute floor by product family, in cubic metres per outlet-month: 1e6 for
# diesel and gasoline, 1e5 for kerosene, biodiesel and GLPA. Nothing below the
# floor is ever flagged, however far it sits from its group's percentile or
# median, which is what keeps the ratio rules off ordinary large outlets.
# The floor does the work. An outlet-month of 1e6 cubic metres is a thousand
# times the largest plausible retail month, so nothing legitimate reaches it;
# a busy station sells tens to a few hundred cubic metres. Kerosene, biodiesel
# and GLPA sell in much smaller volumes, hence the lower floor.
eess_all[, piso_abs_liq := fifelse(
  producto %in% liq_gas_nafta, 1e6,
  fifelse(producto %in% liq_otros, 1e5, NA_real_)
)]

# A liquid-fuel volume is flagged only if it is a retail sale to the public by
# a non-wholesale business, exceeds the floor, and is either more than 10 times
# the group's 99.99th percentile or more than 5,000 times its median.
#
# The rule is narrow on purpose and a tail survives it: a handful of outlets
# still report up to hundreds of thousands of cubic metres a month, which look
# like depot deliveries booked into the retail channel. Section 3 of
# 10_descriptives.R caps those at 3,000 m3 per outlet-month for the quantity
# figures; the rule for the model sample is still open.
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

# 9. Save cleaned1 ----

# Two files come out of this section. eess_all_working_with_aux.rds keeps every
# auxiliary column built above, so the cutoffs and flags can be looked at again
# without recomputing them, and eess_all_cleaned1.rds keeps only the columns the
# raw panel had, with the flagged volumes blanked out in the original character
# variable. cleaned1 is written twice, first with the auxiliary columns and then
# rewritten with the original ones, so a run interrupted in between leaves the
# wider version on disk under the cleaned1 name.

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
library(data.table)
library(fs)

DIR_DATASETS <- DIR_INTERIM

# The file just written still carries the auxiliary columns. That version is
# stored as eess_all_working_with_aux.rds, and eess_all_cleaned1.rds is
# rewritten with the original columns only.
eess_all_aux_from_old_cleaned1 <- readRDS(
  fs::path(DIR_DATASETS, "eess_all_cleaned1.rds")
)

# Original column set
base_original_ref <- readRDS(fs::path(DIR_DATASETS, "eess_all_rawbind_2.rds"))
vars_originales <- names(base_original_ref)

# Final flag: implausibly large GNC or liquid-fuel volume (NA counts as FALSE).
# This is the only volume rule that leaves the script. The intermediate flags of
# sections 4 to 6 travel with the auxiliary file, but nothing is removed on
# their own account.
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

# 10. Very small volumes ----

# The other end of the distribution: positive volumes far below anything a
# month of sales could be, down to 1e-5 cubic metres and less. They are counted
# by order of magnitude, product, sales channel and year, so that it is clear
# whether they are a quirk of one source file or a habit that runs through the
# sample, and then recoded to zero at the end of the section.

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
# product. The bands run by powers of ten because that is the scale on which
# these values differ: a row of 1e-7 and a row of 0.5 cubic metres are not the
# same kind of problem, and only the first is recoded at the end of the section.
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
library(data.table)

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

# Final numeric volume: values in (0, 1e-5] are recoded to 0. The recode makes
# no difference to the panel that leaves this script, because the cut in
# section 11 keeps only volumes of 1e-3 and above and so drops every zero too.
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

# 11. Save cleaned2, cleaned2_cut and cleaned3_cut ----

# cleaned2:     cleaned1 plus volumen_num and volumen_num_final
# cleaned2_cut: cleaned2 without the 229 rows where volumen_num_final is NA
# cleaned3_cut: cleaned2_cut restricted to volumen_num_final >= 1e-3, which drops
#               57,157 rows (zeros included); volumen is overwritten with the
#               final numeric value and the two auxiliary columns are removed
#
# cleaned3_cut is what the rest of the pipeline reads. In it volumen is numeric,
# in cubic metres, and strictly positive. The panel therefore holds no zeros, so
# from here on a month with no row and a month with no sales cannot be told
# apart.

if (!exists("DIR_DATASETS"))
  DIR_DATASETS <- DIR_INTERIM
setDT(eess_all_cleaned1)

saveRDS(eess_all_cleaned1,
        fs::path(DIR_DATASETS, "eess_all_cleaned2.rds"), compress = "gzip")

# What the rules of sections 7 and 8 amount to: 229 volumes nulled out of
# 5,346,549 rows. They are narrow by design, and a tail of large retail
# volumes survives them; see the note on the 1e-3 cut below and the volume
# section of the README.
eess_all_cleaned2_cut <- eess_all_cleaned1[!is.na(volumen_num_final)]
saveRDS(eess_all_cleaned2_cut,
        fs::path(DIR_DATASETS, "eess_all_cleaned2_cut.rds"), compress = "gzip")

# One litre a month. Below that an outlet cannot be selling: the rows are
# zeros and near-zeros that stand for a month with no activity. This drops
# 57,157 rows, and with them every zero, so from here on a month with no sales
# and a month with no record look the same.
eess_all_cleaned3_cut <- eess_all_cleaned2_cut[volumen_num_final >= 1e-3]
eess_all_cleaned3_cut[, volumen := volumen_num_final]
eess_all_cleaned3_cut[, c("volumen_num", "volumen_num_final") := NULL]
saveRDS(eess_all_cleaned3_cut,
        fs::path(DIR_DATASETS, "eess_all_cleaned3_cut.rds"), compress = "gzip")

message("cleaned2, cleaned2_cut and cleaned3_cut saved.")
message("  rows: cleaned2_cut = ", nrow(eess_all_cleaned2_cut),
        " | cleaned3_cut = ", nrow(eess_all_cleaned3_cut))

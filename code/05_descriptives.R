# 05_descriptives.R
# Descriptive evidence on the retail fuel market: the structure of the market
# and how it changed, the price gap between YPF and the private brands, volumes
# and shares, what stations look like, and the pump price against crude and
# import parity.
#
# Input:  the analysis panel, with the department crosswalk from part 3 of
#         03_station_variables.R, and the cost series from 04_market_data.R
# Output: LaTeX tables in <DIR_OUTPUT>/Tablas and figures in
#         <DIR_OUTPUT>/Graficos
#
# Figures are written to a temporary folder first and copied afterwards,
# because the sync client locks files while a plot is being written.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
  library(knitr)
  library(ggplot2)
  library(scales)
})

source("code/00_config.R")

options(scipen = 999)

DIR_OUT_NEW <- DIR_OUTPUT
DIR_TABLES  <- fs::path(DIR_OUT_NEW, "Tablas")
DIR_FIGURES <- fs::path(DIR_OUT_NEW, "Gráficos")
DIR_INPUT   <- DIR_INTERIM

# Part 1. Market structure ----

# Descriptive tables and figures on market structure from the final analysis
# panel: outlets, operators, brands, business types, volumes and prices.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
# Output: 28 LaTeX tables in <DIR_OUTPUT>/Tablas and 15 PNG figures in
#         <DIR_OUTPUT>/Gráficos

options(scipen = 999)

# Paths ----

FILE_BASE <- fs::path(DIR_INPUT, "eess_all_cleaned7_alternative_sinceappearance.rds")

fs::dir_create(DIR_TABLES, recurse = TRUE)
fs::dir_create(DIR_FIGURES, recurse = TRUE)

if (!fs::file_exists(FILE_BASE)) {
  stop("Input file not found: ", FILE_BASE)
}

# Load panel ----

eess <- readRDS(FILE_BASE)
setDT(eess)

cat("Panel loaded\n")
cat("Rows:", nrow(eess), "\n")
cat("Columns:", ncol(eess), "\n")

# Helpers ----

# Normalize free text (accents, case, punctuation, repeated spaces) so that
# operator names that differ only in spelling collapse into one
norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  x
}

# Cell formatting for the exported tables; NA becomes an empty cell
fmt_n <- function(x) {
  ifelse(is.na(x), "", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

save_tex_table <- function(df, file_path, caption = NULL, label = NULL, align = NULL) {
  tex <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = TRUE,
    caption = caption,
    label = label,
    align = align
  )
  writeLines(tex, con = file_path)
}

# Size classes by number of outlets
bucket_fun <- function(x) {
  fifelse(
    x == 1, "1",
    fifelse(
      x %in% 2:3, "2-3",
      fifelse(
        x %in% 4:10, "4-10",
        fifelse(
          x %in% 11:50, "11-50",
          "51+"
        )
      )
    )
  )
}

# Minimal cleaning ----

if (!inherits(eess$periodo_dt, c("IDate", "Date"))) {
  eess[, periodo_dt := as.IDate(periodo_dt)]
}

if (!"anio" %in% names(eess) || all(is.na(eess$anio))) {
  eess[, anio := as.integer(format(periodo_dt, "%Y"))]
}

# Trimmed identifiers. The operator is tracked three ways: raw name, normalized
# name and CUIT (tax id of the firm).
eess[, operador_raw := trimws(as.character(operador))]
eess[, operador_norm := norm_txt(operador)]
eess[, cuit_clean := trimws(as.character(cuit))]
eess[, bandera_clean := trimws(as.character(bandera))]
eess[, producto_clean := trimws(as.character(producto))]
eess[, provincia_clean := trimws(as.character(provincia))]

# Placeholder strings count as missing (for the CUIT, also "0")
eess[cuit_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND", "0"), cuit_clean := NA_character_]
eess[operador_raw %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), operador_raw := NA_character_]
eess[operador_norm %in% c("", "na", "n a", "null", "n d", "nd"), operador_norm := NA_character_]
eess[bandera_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), bandera_clean := NA_character_]
eess[producto_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), producto_clean := NA_character_]
eess[provincia_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), provincia_clean := NA_character_]

# Auxiliary tables ----

# Unique combinations used by the tables below. A boca (outlet) is identified by
# nro_inscripcion; bandera is the brand.

# Outlet-month and outlet-year with their attributes. Product is one of them, so
# an outlet has several rows per period.
boca_mes <- unique(
  eess[!is.na(nro_inscripcion) & !is.na(periodo_dt),
       .(nro_inscripcion, periodo_dt, anio, operador_raw, operador_norm, cuit_clean,
         bandera_clean, producto_clean, provincia_clean)]
)

boca_anio <- unique(
  eess[!is.na(nro_inscripcion) & !is.na(anio),
       .(nro_inscripcion, anio, operador_raw, operador_norm, cuit_clean,
         bandera_clean, producto_clean, provincia_clean)]
)

# CUIT-outlet pairs
cuit_boca <- unique(
  eess[!is.na(cuit_clean) & !is.na(nro_inscripcion),
       .(cuit_clean, nro_inscripcion)]
)

# Operator-outlet pairs, raw and normalized name
operador_boca_raw <- unique(
  eess[!is.na(operador_raw) & !is.na(nro_inscripcion),
       .(operador_raw, nro_inscripcion)]
)

operador_boca_norm <- unique(
  eess[!is.na(operador_norm) & !is.na(nro_inscripcion),
       .(operador_norm, nro_inscripcion)]
)

# CUIT-outlet-brand
cuit_boca_bandera <- unique(
  eess[!is.na(cuit_clean) & !is.na(nro_inscripcion) & !is.na(bandera_clean),
       .(cuit_clean, nro_inscripcion, bandera_clean)]
)

# Outlets, operators and concentration (tables 1-8, figures 1-3) ----

# Table 1: overall snapshot of the panel
tabla_01 <- data.table(
  Estadistica = c(
    "Cantidad de filas",
    "Cantidad de estaciones / bocas",
    "Cantidad de operadores (string raw)",
    "Cantidad de operadores (string normalizado)",
    "Cantidad de operadores (CUIT)",
    "Cantidad de banderas",
    "Cantidad de productos",
    "Cantidad de provincias",
    "Periodo mínimo",
    "Periodo máximo"
  ),
  Valor = c(
    fmt_n(nrow(eess)),
    fmt_n(uniqueN(eess$nro_inscripcion, na.rm = TRUE)),
    fmt_n(uniqueN(eess$operador_raw[!is.na(eess$operador_raw)])),
    fmt_n(uniqueN(eess$operador_norm[!is.na(eess$operador_norm)])),
    fmt_n(uniqueN(eess$cuit_clean[!is.na(eess$cuit_clean)])),
    fmt_n(uniqueN(eess$bandera_clean[!is.na(eess$bandera_clean)])),
    fmt_n(uniqueN(eess$producto_clean[!is.na(eess$producto_clean)])),
    fmt_n(uniqueN(eess$provincia_clean[!is.na(eess$provincia_clean)])),
    as.character(min(eess$periodo_dt, na.rm = TRUE)),
    as.character(max(eess$periodo_dt, na.rm = TRUE))
  )
)

save_tex_table(
  tabla_01,
  fs::path(DIR_TABLES, "01_foto_general_base.tex"),
  caption = "Foto general de la base cleaned7 alternative sinceappearance.",
  label = "tab:foto_general_base"
)

# Table 2: outlets and other basic units per year
tabla_02 <- boca_anio[
  , .(
    n_bocas = uniqueN(nro_inscripcion),
    n_operadores_raw = uniqueN(operador_raw[!is.na(operador_raw)]),
    n_operadores_norm = uniqueN(operador_norm[!is.na(operador_norm)]),
    n_cuit = uniqueN(cuit_clean[!is.na(cuit_clean)]),
    n_banderas = uniqueN(bandera_clean[!is.na(bandera_clean)]),
    n_productos = uniqueN(producto_clean[!is.na(producto_clean)]),
    n_provincias = uniqueN(provincia_clean[!is.na(provincia_clean)])
  ),
  by = anio
][order(anio)]

tabla_02_out <- copy(tabla_02)
num_cols_02 <- setdiff(names(tabla_02_out), "anio")
for (v in num_cols_02) tabla_02_out[, (v) := fmt_n(get(v))]

save_tex_table(
  tabla_02_out,
  fs::path(DIR_TABLES, "02_bocas_por_anio.tex"),
  caption = "Cantidad de bocas y otras unidades básicas por año.",
  label = "tab:bocas_por_anio"
)

# Table 3: top 20 CUITs by number of outlets
cuit_counts <- cuit_boca[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = cuit_clean
][order(-n_bocas, cuit_clean)]

# Main operator name and main brand of each CUIT: the one covering the most
# outlets, ties broken alphabetically
main_operador_cuit <- unique(
  eess[!is.na(cuit_clean) & !is.na(nro_inscripcion) & !is.na(operador_norm),
       .(cuit_clean, nro_inscripcion, operador_norm)]
)[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(cuit_clean, operador_norm)
][order(cuit_clean, -n_bocas, operador_norm)][
  , .SD[1],
  by = cuit_clean
]

main_bandera_cuit <- cuit_boca_bandera[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(cuit_clean, bandera_clean)
][order(cuit_clean, -n_bocas, bandera_clean)][
  , .SD[1],
  by = cuit_clean
]

tabla_03 <- cuit_counts[1:min(20, .N)]
tabla_03[, rank := .I]
tabla_03[main_operador_cuit, operador_principal := i.operador_norm, on = "cuit_clean"]
tabla_03[main_bandera_cuit, bandera_principal := i.bandera_clean, on = "cuit_clean"]
setcolorder(tabla_03, c("rank", "cuit_clean", "n_bocas", "operador_principal", "bandera_principal"))
setnames(tabla_03, c("rank", "cuit_clean", "n_bocas"), c("Rank", "CUIT", "Bocas"))

tabla_03_out <- copy(tabla_03)
tabla_03_out[, Bocas := fmt_n(Bocas)]

save_tex_table(
  tabla_03_out,
  fs::path(DIR_TABLES, "03_top20_cuit_por_bocas.tex"),
  caption = "Top 20 CUIT por cantidad de bocas.",
  label = "tab:top20_cuit_bocas"
)

# Table 4: top 20 operators by number of outlets, raw name
tabla_04 <- operador_boca_raw[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = operador_raw
][order(-n_bocas, operador_raw)][1:min(20, .N)]

tabla_04[, rank := .I]
setcolorder(tabla_04, c("rank", "operador_raw", "n_bocas"))
setnames(tabla_04, c("rank", "operador_raw", "n_bocas"), c("Rank", "Operador", "Bocas"))

tabla_04_out <- copy(tabla_04)
tabla_04_out[, Bocas := fmt_n(Bocas)]

save_tex_table(
  tabla_04_out,
  fs::path(DIR_TABLES, "04_top20_operador_raw_por_bocas.tex"),
  caption = "Top 20 operadores (string raw) por cantidad de bocas.",
  label = "tab:top20_operador_raw_bocas"
)

# Table 5: top 20 operators by number of outlets, normalized name

# Number of distinct raw spellings behind each normalized name
nombres_raw_por_norm <- unique(
  eess[!is.na(operador_raw) & !is.na(operador_norm),
       .(operador_norm, operador_raw)]
)[
  , .(n_nombres_raw = .N),
  by = operador_norm
]

tabla_05 <- operador_boca_norm[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = operador_norm
][order(-n_bocas, operador_norm)][1:min(20, .N)]

tabla_05[nombres_raw_por_norm, n_nombres_raw := i.n_nombres_raw, on = "operador_norm"]
tabla_05[, rank := .I]
setcolorder(tabla_05, c("rank", "operador_norm", "n_bocas", "n_nombres_raw"))
setnames(tabla_05,
         c("rank", "operador_norm", "n_bocas", "n_nombres_raw"),
         c("Rank", "Operador_normalizado", "Bocas", "Nombres_raw_asociados"))

tabla_05_out <- copy(tabla_05)
tabla_05_out[, Bocas := fmt_n(Bocas)]
tabla_05_out[, Nombres_raw_asociados := fmt_n(Nombres_raw_asociados)]

save_tex_table(
  tabla_05_out,
  fs::path(DIR_TABLES, "05_top20_operador_norm_por_bocas.tex"),
  caption = "Top 20 operadores normalizados por cantidad de bocas.",
  label = "tab:top20_operador_norm_bocas"
)

# Table 6: concentration of outlets by CUIT size class.
# The bucket column is added to cuit_counts by reference and stays there.
tabla_06 <- cuit_counts[
  , bucket := bucket_fun(n_bocas)
][
  , .(
    n_cuit = .N,
    bocas_asociadas = sum(n_bocas)
  ),
  by = bucket
][
  , `:=`(
    share_cuit = n_cuit / sum(n_cuit),
    share_bocas_asociadas = bocas_asociadas / sum(bocas_asociadas)
  )
][match(c("1", "2-3", "4-10", "11-50", "51+"), bucket)]

tabla_06_out <- copy(tabla_06)
tabla_06_out[, n_cuit := fmt_n(n_cuit)]
tabla_06_out[, bocas_asociadas := fmt_n(bocas_asociadas)]
tabla_06_out[, share_cuit := fmt_pct(share_cuit)]
tabla_06_out[, share_bocas_asociadas := fmt_pct(share_bocas_asociadas)]

setnames(tabla_06_out,
         c("bucket", "n_cuit", "bocas_asociadas", "share_cuit", "share_bocas_asociadas"),
         c("Bucket_bocas", "Cantidad_CUIT", "Bocas_asociadas", "Share_CUIT", "Share_bocas_asociadas"))

save_tex_table(
  tabla_06_out,
  fs::path(DIR_TABLES, "06_buckets_concentracion_cuit.tex"),
  caption = "Buckets de concentración de bocas por CUIT.",
  label = "tab:buckets_cuit"
)

# Table 7: concentration of outlets by size class of the normalized operator
operador_norm_counts <- operador_boca_norm[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = operador_norm
][order(-n_bocas, operador_norm)]

tabla_07 <- operador_norm_counts[
  , bucket := bucket_fun(n_bocas)
][
  , .(
    n_operadores = .N,
    bocas_asociadas = sum(n_bocas)
  ),
  by = bucket
][
  , `:=`(
    share_operadores = n_operadores / sum(n_operadores),
    share_bocas_asociadas = bocas_asociadas / sum(bocas_asociadas)
  )
][match(c("1", "2-3", "4-10", "11-50", "51+"), bucket)]

tabla_07_out <- copy(tabla_07)
tabla_07_out[, n_operadores := fmt_n(n_operadores)]
tabla_07_out[, bocas_asociadas := fmt_n(bocas_asociadas)]
tabla_07_out[, share_operadores := fmt_pct(share_operadores)]
tabla_07_out[, share_bocas_asociadas := fmt_pct(share_bocas_asociadas)]

setnames(tabla_07_out,
         c("bucket", "n_operadores", "bocas_asociadas", "share_operadores", "share_bocas_asociadas"),
         c("Bucket_bocas", "Cantidad_operadores", "Bocas_asociadas", "Share_operadores", "Share_bocas_asociadas"))

save_tex_table(
  tabla_07_out,
  fs::path(DIR_TABLES, "07_buckets_concentracion_operador_norm.tex"),
  caption = "Buckets de concentración de bocas por operador normalizado.",
  label = "tab:buckets_operador_norm"
)

# Table 8: three main brands within each of the 10 largest CUITs
top10_cuit <- cuit_counts[1:min(10, .N)]
top10_cuit[, rank_cuit := .I]

tabla_08 <- cuit_boca_bandera[
  top10_cuit[, .(cuit_clean, rank_cuit)],
  on = "cuit_clean",
  nomatch = 0
][
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(rank_cuit, cuit_clean, bandera_clean)
][
  order(rank_cuit, -n_bocas, bandera_clean)
][
  , rank_bandera := seq_len(.N),
  by = cuit_clean
][rank_bandera <= 3]

total_bocas_top10 <- top10_cuit[, .(cuit_clean, total_bocas_cuit = n_bocas)]
tabla_08[total_bocas_top10, total_bocas_cuit := i.total_bocas_cuit, on = "cuit_clean"]
tabla_08[, share_bocas_cuit := n_bocas / total_bocas_cuit]

setcolorder(tabla_08, c("rank_cuit", "cuit_clean", "rank_bandera", "bandera_clean", "n_bocas", "share_bocas_cuit"))
setnames(tabla_08,
         c("rank_cuit", "cuit_clean", "rank_bandera", "bandera_clean", "n_bocas", "share_bocas_cuit"),
         c("Rank_CUIT", "CUIT", "Rank_bandera", "Bandera", "Bocas", "Share_dentro_CUIT"))

tabla_08_out <- copy(tabla_08)
tabla_08_out[, Bocas := fmt_n(Bocas)]
tabla_08_out[, Share_dentro_CUIT := fmt_pct(Share_dentro_CUIT)]

save_tex_table(
  tabla_08_out,
  fs::path(DIR_TABLES, "08_principales_banderas_top10_cuit.tex"),
  caption = "Principales banderas dentro de los 10 CUIT con más bocas.",
  label = "tab:banderas_top10_cuit"
)

# Figure 1: outlets per year
g1_data <- copy(tabla_02)

g1 <- ggplot(g1_data, aes(x = anio, y = n_bocas)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  labs(
    x = "Año",
    y = "Cantidad de bocas",
    title = "Bocas observadas por año"
  ) +
  theme_minimal()

ggsave(
  filename = fs::path(DIR_FIGURES, "g1_bocas_por_anio.png"),
  plot = g1,
  width = 8,
  height = 5,
  dpi = 300
)

# Figure 2: top 20 CUITs by number of outlets
g2_data <- copy(cuit_counts[1:min(20, .N)])
g2_data[, cuit_clean := factor(cuit_clean, levels = rev(cuit_clean))]

g2 <- ggplot(g2_data, aes(x = cuit_clean, y = n_bocas)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "CUIT",
    y = "Cantidad de bocas",
    title = "Top 20 CUIT por cantidad de bocas"
  ) +
  theme_minimal()

ggsave(
  filename = fs::path(DIR_FIGURES, "g2_top20_cuit_por_bocas.png"),
  plot = g2,
  width = 8,
  height = 7,
  dpi = 300
)

# Figure 3: top 20 normalized operators by number of outlets
g3_data <- copy(operador_norm_counts[1:min(20, .N)])
g3_data[, operador_norm := factor(operador_norm, levels = rev(operador_norm))]

g3 <- ggplot(g3_data, aes(x = operador_norm, y = n_bocas)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Operador normalizado",
    y = "Cantidad de bocas",
    title = "Top 20 operadores normalizados por cantidad de bocas"
  ) +
  theme_minimal()

ggsave(
  filename = fs::path(DIR_FIGURES, "g3_top20_operador_norm_por_bocas.png"),
  plot = g3,
  width = 8,
  height = 7,
  dpi = 300
)

cat("\nDone. Files written:\n")
cat("- 01_foto_general_base.tex\n")
cat("- 02_bocas_por_anio.tex\n")
cat("- 03_top20_cuit_por_bocas.tex\n")
cat("- 04_top20_operador_raw_por_bocas.tex\n")
cat("- 05_top20_operador_norm_por_bocas.tex\n")
cat("- 06_buckets_concentracion_cuit.tex\n")
cat("- 07_buckets_concentracion_operador_norm.tex\n")
cat("- 08_principales_banderas_top10_cuit.tex\n")
cat("- g1_bocas_por_anio.png\n")
cat("- g2_top20_cuit_por_bocas.png\n")
cat("- g3_top20_operador_norm_por_bocas.png\n")

# Outlets over time and business type (tables 9-12, figures 4-7) ----

req_vars_time <- c("nro_inscripcion", "periodo_dt", "anio", "tipo_negocio", "tipo_negocio_raw")
stopifnot(all(req_vars_time %in% names(eess)))

if (!inherits(eess$periodo_dt, c("IDate", "Date"))) {
  eess[, periodo_dt := as.IDate(periodo_dt)]
}

# tipo_negocio is the harmonized business type; tipo_negocio_raw is the type as
# reported in the source
eess[, tipo_negocio_clean := trimws(as.character(tipo_negocio))]
eess[, tipo_negocio_raw_clean := trimws(as.character(tipo_negocio_raw))]

eess[tipo_negocio_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), tipo_negocio_clean := NA_character_]
eess[tipo_negocio_raw_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), tipo_negocio_raw_clean := NA_character_]

# Plot theme used from figure 4 on: white background, legend at the bottom
theme_paper <- function() {
  theme_bw(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.width = grid::unit(1.1, "cm"),
      legend.spacing.x = grid::unit(0.15, "cm")
    )
}

# Outlet-month tables with the harmonized and the raw business type
boca_mes_tipo_h <- unique(
  eess[
    !is.na(nro_inscripcion) & !is.na(periodo_dt) & !is.na(tipo_negocio_clean),
    .(periodo_dt, anio, nro_inscripcion, tipo_negocio_clean)
  ]
)

boca_mes_tipo_raw <- unique(
  eess[
    !is.na(nro_inscripcion) & !is.na(periodo_dt) & !is.na(tipo_negocio_raw_clean),
    .(periodo_dt, anio, nro_inscripcion, tipo_negocio_raw_clean)
  ]
)

# An outlet-month should carry a single business type, harmonized or raw
check_h <- boca_mes_tipo_h[
  , .(n_tipos = uniqueN(tipo_negocio_clean)),
  by = .(nro_inscripcion, periodo_dt)
][n_tipos > 1]

cat("\nHarmonized type, outlet-months with more than one type:", nrow(check_h), "\n")

check_raw <- boca_mes_tipo_raw[
  , .(n_tipos = uniqueN(tipo_negocio_raw_clean)),
  by = .(nro_inscripcion, periodo_dt)
][n_tipos > 1]

cat("Raw type, outlet-months with more than one type:", nrow(check_raw), "\n")

# Table 9: outlets observed per month
tabla_09 <- boca_mes_tipo_h[
  , .(bocas_mes = uniqueN(nro_inscripcion)),
  by = .(periodo_dt, anio)
][order(periodo_dt)]

tabla_09_out <- copy(tabla_09)
tabla_09_out[, periodo := format(periodo_dt, "%Y-%m")]
tabla_09_out[, bocas_mes := fmt_n(bocas_mes)]
tabla_09_out <- tabla_09_out[, .(periodo, anio, bocas_mes)]

save_tex_table(
  tabla_09_out,
  fs::path(DIR_TABLES, "09_bocas_por_mes.tex"),
  caption = "Cantidad de bocas observadas por mes.",
  label = "tab:bocas_por_mes"
)

# Figure 4: monthly series of observed outlets
g4 <- ggplot(tabla_09, aes(x = as.Date(periodo_dt), y = bocas_mes)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Fecha",
    y = "Cantidad de bocas",
    title = "Bocas observadas por mes"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g4_bocas_por_mes.png"),
  plot = g4,
  width = 9,
  height = 5.5,
  dpi = 320,
  bg = "white"
)

# Monthly composition of outlets by harmonized business type
comp_h <- boca_mes_tipo_h[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(periodo_dt, tipo_negocio_clean)
]

tot_h <- boca_mes_tipo_h[
  , .(total_bocas = uniqueN(nro_inscripcion)),
  by = periodo_dt
]

comp_h <- tot_h[comp_h, on = "periodo_dt"]
comp_h[, share := n_bocas / total_bocas]

# Types ordered by average share, so that tables and legends come out sorted
order_h <- comp_h[
  , .(share_promedio = mean(share, na.rm = TRUE)),
  by = tipo_negocio_clean
][order(-share_promedio)]

comp_h[, tipo_negocio_clean := factor(tipo_negocio_clean, levels = order_h$tipo_negocio_clean)]

# Same composition with the raw business type
comp_raw <- boca_mes_tipo_raw[
  , .(n_bocas = uniqueN(nro_inscripcion)),
  by = .(periodo_dt, tipo_negocio_raw_clean)
]

tot_raw <- boca_mes_tipo_raw[
  , .(total_bocas = uniqueN(nro_inscripcion)),
  by = periodo_dt
]

comp_raw <- tot_raw[comp_raw, on = "periodo_dt"]
comp_raw[, share := n_bocas / total_bocas]

order_raw <- comp_raw[
  , .(share_promedio = mean(share, na.rm = TRUE)),
  by = tipo_negocio_raw_clean
][order(-share_promedio)]

comp_raw[, tipo_negocio_raw_clean := factor(tipo_negocio_raw_clean, levels = order_raw$tipo_negocio_raw_clean)]

# Short legend labels for the long category names, harmonized type
comp_h[, tipo_plot := as.character(tipo_negocio_clean)]

comp_h[tipo_plot == "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
       tipo_plot := "Líquidos"]

comp_h[tipo_plot == "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
       tipo_plot := "Duales L+GNC"]

comp_h[tipo_plot == "Bocas de expendio (venta por menor) Sólo GNC",
       tipo_plot := "Sólo GNC"]

comp_h[tipo_plot == "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
       tipo_plot := "Líquidos + PRVE"]

comp_h[tipo_plot == "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
       tipo_plot := "Duales L+GLPA"]

comp_h[tipo_plot == "Boca de expendio de sólo GLPA",
       tipo_plot := "Sólo GLPA"]

comp_h[tipo_plot == "Distribuidor (con camiones)",
       tipo_plot := "Distribuidor"]

comp_h[tipo_plot == "Revendedor general (venta a granel mayorista, incluye tambores)",
       tipo_plot := "Revendedor general"]

comp_h[tipo_plot == "Comercializador (venta a granel mayorista)",
       tipo_plot := "Comercializador"]

# Same short labels for the raw type
comp_raw[, tipo_plot_raw := as.character(tipo_negocio_raw_clean)]

comp_raw[tipo_plot_raw == "Bocas de expendio (venta por menor) Combustibles líquidos únicamente",
         tipo_plot_raw := "Líquidos"]

comp_raw[tipo_plot_raw == "Bocas de expendio (venta por menor) Duales (líquidos + GNC)",
         tipo_plot_raw := "Duales L+GNC"]

comp_raw[tipo_plot_raw == "Bocas de expendio (venta por menor) Sólo GNC",
         tipo_plot_raw := "Sólo GNC"]

comp_raw[tipo_plot_raw == "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE",
         tipo_plot_raw := "Líquidos + PRVE"]

comp_raw[tipo_plot_raw == "Bocas de expendio (venta por menor) Duales (líquidos + GLPA)",
         tipo_plot_raw := "Duales L+GLPA"]

comp_raw[tipo_plot_raw == "Boca de expendio de sólo GLPA",
         tipo_plot_raw := "Sólo GLPA"]

comp_raw[tipo_plot_raw == "Distribuidor (con camiones)",
         tipo_plot_raw := "Distribuidor"]

comp_raw[tipo_plot_raw == "Revendedor general (venta a granel mayorista, incluye tambores)",
         tipo_plot_raw := "Revendedor general"]

comp_raw[tipo_plot_raw == "Comercializador (venta a granel mayorista)",
         tipo_plot_raw := "Comercializador"]

# Legend order by average share
order_h_plot <- comp_h[
  , .(share_promedio = mean(share, na.rm = TRUE)),
  by = tipo_plot
][order(-share_promedio)]

comp_h[, tipo_plot := factor(tipo_plot, levels = order_h_plot$tipo_plot)]

order_raw_plot <- comp_raw[
  , .(share_promedio = mean(share, na.rm = TRUE)),
  by = tipo_plot_raw
][order(-share_promedio)]

comp_raw[, tipo_plot_raw := factor(tipo_plot_raw, levels = order_raw_plot$tipo_plot_raw)]

# Table 10: composition by harmonized type in the last 12 months of the panel.
# Each cell shows the number of outlets and the monthly share.
ultimos_12 <- sort(unique(comp_h$periodo_dt), decreasing = TRUE)[1:12]
ultimos_12 <- sort(ultimos_12)

comp_h_12 <- comp_h[periodo_dt %in% ultimos_12]

tipos_h <- levels(comp_h$tipo_negocio_clean)

tabla_10_counts <- dcast(
  comp_h_12,
  periodo_dt + total_bocas ~ tipo_negocio_clean,
  value.var = "n_bocas",
  fill = 0
)

tabla_10_shares <- dcast(
  comp_h_12,
  periodo_dt ~ tipo_negocio_clean,
  value.var = "share",
  fill = 0
)

tabla_10 <- data.table(
  periodo_dt = tabla_10_counts$periodo_dt,
  periodo = format(tabla_10_counts$periodo_dt, "%Y-%m"),
  total_bocas = fmt_n(tabla_10_counts$total_bocas)
)

for (tt in tipos_h) {
  n_vec <- tabla_10_counts[[tt]]
  s_vec <- tabla_10_shares[[tt]]
  tabla_10[, (tt) := paste0(fmt_n(n_vec), " (", fmt_pct(s_vec), ")")]
}

tabla_10 <- tabla_10[, c("periodo", "total_bocas", tipos_h), with = FALSE]
setnames(tabla_10, "total_bocas", "Total_bocas")

save_tex_table(
  tabla_10,
  fs::path(DIR_TABLES, "10_ultimos12_composicion_tipo_negocio.tex"),
  caption = "Composición de bocas por tipo de negocio armonizado en los últimos 12 meses. Cada celda muestra cantidad de bocas y share mensual.",
  label = "tab:ultimos12_tipo_negocio"
)

# Table 11: same table with the raw type
comp_raw_12 <- comp_raw[periodo_dt %in% ultimos_12]

tipos_raw <- levels(comp_raw$tipo_negocio_raw_clean)

tabla_11_counts <- dcast(
  comp_raw_12,
  periodo_dt + total_bocas ~ tipo_negocio_raw_clean,
  value.var = "n_bocas",
  fill = 0
)

tabla_11_shares <- dcast(
  comp_raw_12,
  periodo_dt ~ tipo_negocio_raw_clean,
  value.var = "share",
  fill = 0
)

tabla_11 <- data.table(
  periodo_dt = tabla_11_counts$periodo_dt,
  periodo = format(tabla_11_counts$periodo_dt, "%Y-%m"),
  total_bocas = fmt_n(tabla_11_counts$total_bocas)
)

for (tt in tipos_raw) {
  n_vec <- tabla_11_counts[[tt]]
  s_vec <- tabla_11_shares[[tt]]
  tabla_11[, (tt) := paste0(fmt_n(n_vec), " (", fmt_pct(s_vec), ")")]
}

tabla_11 <- tabla_11[, c("periodo", "total_bocas", tipos_raw), with = FALSE]
setnames(tabla_11, "total_bocas", "Total_bocas")

save_tex_table(
  tabla_11,
  fs::path(DIR_TABLES, "11_ultimos12_composicion_tipo_negocio_raw.tex"),
  caption = "Composición de bocas por tipo de negocio original en los últimos 12 meses. Cada celda muestra cantidad de bocas y share mensual.",
  label = "tab:ultimos12_tipo_negocio_raw"
)

# Figure 5: stacked area of monthly shares, harmonized type
g5 <- ggplot(comp_h, aes(x = as.Date(periodo_dt), y = share, fill = tipo_plot)) +
  geom_area(color = "white", linewidth = 0.15, alpha = 0.95) +
  labs(
    x = "Fecha",
    y = "Share de bocas",
    title = "Composición mensual de bocas por tipo de negocio",
    subtitle = "Variable armonizada"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g5_area_share_tipo_negocio.png"),
  plot = g5,
  width = 10,
  height = 6.5,
  dpi = 320,
  bg = "white"
)

# Figure 6: stacked area of outlet counts, harmonized type
g6 <- ggplot(comp_h, aes(x = as.Date(periodo_dt), y = n_bocas, fill = tipo_plot)) +
  geom_area(color = "white", linewidth = 0.15, alpha = 0.95) +
  labs(
    x = "Fecha",
    y = "Cantidad de bocas",
    title = "Composición mensual de bocas por tipo de negocio",
    subtitle = "Variable armonizada - niveles"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g6_area_niveles_tipo_negocio.png"),
  plot = g6,
  width = 10,
  height = 6.5,
  dpi = 320,
  bg = "white"
)

# Figure 7: stacked area of monthly shares, raw type
g7 <- ggplot(comp_raw, aes(x = as.Date(periodo_dt), y = share, fill = tipo_plot_raw)) +
  geom_area(color = "white", linewidth = 0.15, alpha = 0.95) +
  labs(
    x = "Fecha",
    y = "Share de bocas",
    title = "Composición mensual de bocas por tipo de negocio",
    subtitle = "Variable original"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g7_area_share_tipo_negocio_raw.png"),
  plot = g7,
  width = 10,
  height = 6.5,
  dpi = 320,
  bg = "white"
)

# Table 12: average, minimum and maximum monthly share by harmonized type
tabla_12 <- comp_h[
  , .(
    share_promedio = mean(share, na.rm = TRUE),
    share_min = min(share, na.rm = TRUE),
    share_max = max(share, na.rm = TRUE)
  ),
  by = tipo_negocio_clean
][order(-share_promedio)]

tabla_12_out <- copy(tabla_12)
tabla_12_out[, share_promedio := fmt_pct(share_promedio)]
tabla_12_out[, share_min := fmt_pct(share_min)]
tabla_12_out[, share_max := fmt_pct(share_max)]

setnames(
  tabla_12_out,
  c("tipo_negocio_clean", "share_promedio", "share_min", "share_max"),
  c("Tipo_negocio", "Share_promedio", "Share_min", "Share_max")
)

save_tex_table(
  tabla_12_out,
  fs::path(DIR_TABLES, "12_shares_promedio_tipo_negocio.tex"),
  caption = "Shares mensuales promedio, mínimos y máximos de bocas por tipo de negocio armonizado.",
  label = "tab:shares_promedio_tipo_negocio"
)

cat("\nDone. Files written in this block:\n")
cat("- 09_bocas_por_mes.tex\n")
cat("- 10_ultimos12_composicion_tipo_negocio.tex\n")
cat("- 11_ultimos12_composicion_tipo_negocio_raw.tex\n")
cat("- 12_shares_promedio_tipo_negocio.tex\n")
cat("- g4_bocas_por_mes.png\n")
cat("- g5_area_share_tipo_negocio.png\n")
cat("- g6_area_niveles_tipo_negocio.png\n")
cat("- g7_area_share_tipo_negocio_raw.png\n")

# Missing values, zeros and volume (tables 13-16) ----

# Prices and volume may come as character; anything that does not parse is NA
parse_num_safe <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "N/D", "ND")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}

eess[, precio_surtidor_num := parse_num_safe(precio_surtidor)]
eess[, precio_con_impuestos_num := parse_num_safe(precio_con_impuestos)]
eess[, precio_sin_impuestos_num := parse_num_safe(precio_sin_impuestos)]

eess[, volumen_num := parse_num_safe(volumen)]

# Table 13: share of missing values and zeros in prices and volume.
# Shares of zeros are taken over non-missing observations.
n_total_obs <- nrow(eess)

tabla_13 <- data.table(
  Indicador = c(
    "Share missing en precio_surtidor",
    "Share missing en precio_con_impuestos",
    "Share missing en precio_sin_impuestos",
    "Share missing en volumen",
    "Share volumen == 0",
    "Share precio_surtidor == 0"
  ),
  Valor = c(
    mean(is.na(eess$precio_surtidor_num)),
    mean(is.na(eess$precio_con_impuestos_num)),
    mean(is.na(eess$precio_sin_impuestos_num)),
    mean(is.na(eess$volumen_num)),
    mean(eess$volumen_num == 0, na.rm = TRUE),
    mean(eess$precio_surtidor_num == 0, na.rm = TRUE)
  )
)

tabla_13_out <- copy(tabla_13)
tabla_13_out[, Valor := fmt_pct(Valor, digits = 2)]

save_tex_table(
  tabla_13_out,
  fs::path(DIR_TABLES, "13_missings_y_ceros_precio_volumen.tex"),
  caption = "Shares de missing y ceros en variables de precios y volumen.",
  label = "tab:missings_ceros_precio_volumen"
)

# Table 14: summary statistics of volume
vol_ok <- eess[!is.na(volumen_num), volumen_num]

tabla_14 <- data.table(
  Estadistica = c(
    "n_total",
    "Observaciones con volumen_num no missing",
    "Share de parseo fallido / missing",
    "Media",
    "Desvío estándar",
    "Mínimo",
    "p01",
    "p05",
    "p10",
    "p25",
    "Mediana",
    "p75",
    "p90",
    "p95",
    "p99",
    "Máximo",
    "Share de ceros"
  ),
  Valor = c(
    n_total_obs,
    sum(!is.na(eess$volumen_num)),
    mean(is.na(eess$volumen_num)),
    mean(vol_ok),
    sd(vol_ok),
    min(vol_ok),
    quantile(vol_ok, 0.01, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.05, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.10, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.25, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.50, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.75, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.90, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.95, na.rm = TRUE, names = FALSE),
    quantile(vol_ok, 0.99, na.rm = TRUE, names = FALSE),
    max(vol_ok),
    mean(vol_ok == 0, na.rm = TRUE)
  )
)

tabla_14_out <- copy(tabla_14)

# Counts, shares and the remaining statistics each get their own format
tabla_14_out[Estadistica %in% c("n_total", "Observaciones con volumen_num no missing"),
             Valor_fmt := fmt_n(as.numeric(Valor))]

tabla_14_out[Estadistica %in% c("Share de parseo fallido / missing", "Share de ceros"),
             Valor_fmt := fmt_pct(as.numeric(Valor), digits = 2)]

tabla_14_out[!Estadistica %in% c("n_total", "Observaciones con volumen_num no missing",
                                 "Share de parseo fallido / missing", "Share de ceros"),
             Valor_fmt := fmt_num(as.numeric(Valor), digits = 3)]

tabla_14_out <- tabla_14_out[, .(Estadistica, Valor = Valor_fmt)]

save_tex_table(
  tabla_14_out,
  fs::path(DIR_TABLES, "14_resumen_descriptivo_volumen.tex"),
  caption = "Resumen descriptivo de la variable volumen.",
  label = "tab:resumen_volumen"
)

# Table 15: the 25 records with the largest volume
tabla_15 <- eess[
  !is.na(volumen_num),
  .(
    periodo = format(periodo_dt, "%Y-%m"),
    nro_inscripcion,
    operador,
    bandera,
    provincia,
    producto,
    canal_de_comercializacion,
    tipo_negocio,
    volumen_original = as.character(volumen),
    volumen_num
  )
][order(-volumen_num)][1:25]

tabla_15_out <- copy(tabla_15)
tabla_15_out[, volumen_num := fmt_num(volumen_num, digits = 3)]
setnames(tabla_15_out, "volumen_num", "volumen_num_parseado")

save_tex_table(
  tabla_15_out,
  fs::path(DIR_TABLES, "15_top25_volumen.tex"),
  caption = "Top 25 observaciones por volumen.",
  label = "tab:top25_volumen"
)

# Table 16: the 25 records with the smallest volume
tabla_16 <- eess[
  !is.na(volumen_num),
  .(
    periodo = format(periodo_dt, "%Y-%m"),
    nro_inscripcion,
    operador,
    bandera,
    provincia,
    producto,
    canal_de_comercializacion,
    tipo_negocio,
    volumen_original = as.character(volumen),
    volumen_num
  )
][order(volumen_num)][1:25]

tabla_16_out <- copy(tabla_16)
tabla_16_out[, volumen_num := fmt_num(volumen_num, digits = 6)]
setnames(tabla_16_out, "volumen_num", "volumen_num_parseado")

save_tex_table(
  tabla_16_out,
  fs::path(DIR_TABLES, "16_bottom25_volumen.tex"),
  caption = "Bottom 25 observaciones por volumen.",
  label = "tab:bottom25_volumen"
)

cat("\nDone. Files written in this block:\n")
cat("- 13_missings_y_ceros_precio_volumen.tex\n")
cat("- 14_resumen_descriptivo_volumen.tex\n")
cat("- 15_top25_volumen.tex\n")
cat("- 16_bottom25_volumen.tex\n")

# Volume and prices by group (tables 17-24, figures 8-11) ----

# Rebuild the numeric and cleaned columns if the blocks above were skipped
if (!"volumen_num" %in% names(eess)) {
  eess[, volumen_num := parse_num_safe(volumen)]
}
if (!"precio_surtidor_num" %in% names(eess)) {
  eess[, precio_surtidor_num := parse_num_safe(precio_surtidor)]
}

if (!"producto_clean" %in% names(eess)) {
  eess[, producto_clean := trimws(as.character(producto))]
  eess[producto_clean %in% c("", "NA", "N/A", "NULL", "N/D", "ND"), producto_clean := NA_character_]
}

# Broad product groups from the normalized product name: GNC (compressed natural
# gas), Naftas (gasoline) and Gasoil (diesel). Any other product is left out.
eess[, producto_norm2 := norm_txt(producto_clean)]

eess[, grupo_producto := fifelse(
  grepl("gnc", producto_norm2),
  "GNC",
  fifelse(
    grepl("nafta", producto_norm2),
    "Naftas",
    fifelse(
      grepl("gas oil|gasoil|diesel", producto_norm2),
      "Gasoil",
      NA_character_
    )
  )
)]

eess_gp <- eess[!is.na(grupo_producto)]

eess_gp[, grupo_producto := factor(grupo_producto, levels = c("Naftas", "GNC", "Gasoil"))]

# Table 17: volume by broad product group
tabla_17 <- eess_gp[
  !is.na(volumen_num),
  .(
    n = .N,
    media = mean(volumen_num),
    sd = sd(volumen_num),
    p50 = quantile(volumen_num, 0.50, na.rm = TRUE, names = FALSE),
    p90 = quantile(volumen_num, 0.90, na.rm = TRUE, names = FALSE),
    p95 = quantile(volumen_num, 0.95, na.rm = TRUE, names = FALSE),
    p99 = quantile(volumen_num, 0.99, na.rm = TRUE, names = FALSE),
    maximo = max(volumen_num),
    share_ceros = mean(volumen_num == 0, na.rm = TRUE)
  ),
  by = grupo_producto
][order(grupo_producto)]

tabla_17_out <- copy(tabla_17)
tabla_17_out[, n := fmt_n(n)]
tabla_17_out[, media := fmt_num(media, digits = 3)]
tabla_17_out[, sd := fmt_num(sd, digits = 3)]
tabla_17_out[, p50 := fmt_num(p50, digits = 3)]
tabla_17_out[, p90 := fmt_num(p90, digits = 3)]
tabla_17_out[, p95 := fmt_num(p95, digits = 3)]
tabla_17_out[, p99 := fmt_num(p99, digits = 3)]
tabla_17_out[, maximo := fmt_num(maximo, digits = 3)]
tabla_17_out[, share_ceros := fmt_pct(share_ceros, digits = 2)]

setnames(
  tabla_17_out,
  c("grupo_producto", "n", "media", "sd", "p50", "p90", "p95", "p99", "maximo", "share_ceros"),
  c("Grupo", "n", "Media", "Desvío_estándar", "p50", "p90", "p95", "p99", "Máximo", "Share_ceros")
)

save_tex_table(
  tabla_17_out,
  fs::path(DIR_TABLES, "17_volumen_por_grandes_grupos.tex"),
  caption = "Descriptivos de volumen por grandes grupos de producto.",
  label = "tab:volumen_grandes_grupos"
)

# Figure 8: histogram of volume in levels, by product group
g8 <- ggplot(
  eess_gp[!is.na(volumen_num)],
  aes(x = volumen_num)
) +
  geom_histogram(bins = 60, fill = "grey70", color = "white") +
  facet_wrap(~ grupo_producto, scales = "free_y") +
  labs(
    x = "Volumen",
    y = "Frecuencia",
    title = "Distribución del volumen por grandes grupos de producto",
    subtitle = "Histogramas en niveles"
  ) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g8_hist_volumen_grupos_niveles.png"),
  plot = g8,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)

# Figure 9: histogram of log10(volume), positive volumes only
g9 <- ggplot(
  eess_gp[!is.na(volumen_num) & volumen_num > 0],
  aes(x = log10(volumen_num))
) +
  geom_histogram(bins = 60, fill = "grey70", color = "white") +
  facet_wrap(~ grupo_producto, scales = "free_y") +
  labs(
    x = expression(log[10](volumen)),
    y = "Frecuencia",
    title = "Distribución del volumen por grandes grupos de producto",
    subtitle = "Histogramas de log10(volumen) para observaciones positivas"
  ) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g9_hist_log10_volumen_grupos.png"),
  plot = g9,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)

# Figure 10: histogram of log(1 + volume), which keeps the zeros
g10 <- ggplot(
  eess_gp[!is.na(volumen_num)],
  aes(x = log1p(volumen_num))
) +
  geom_histogram(bins = 60, fill = "grey70", color = "white") +
  facet_wrap(~ grupo_producto, scales = "free_y") +
  labs(
    x = "log(1 + volumen)",
    y = "Frecuencia",
    title = "Distribución del volumen por grandes grupos de producto",
    subtitle = "Histogramas de log(1 + volumen)"
  ) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g10_hist_log1p_volumen_grupos.png"),
  plot = g10,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)

# Figure 11: total monthly volume by product group. tabla_18 is only plotted, so
# from here on object numbers run one ahead of the exported file numbers.
tabla_18 <- eess_gp[
  !is.na(periodo_dt) & !is.na(volumen_num),
  .(volumen_total = sum(volumen_num, na.rm = TRUE)),
  by = .(periodo_dt, grupo_producto)
][order(periodo_dt, grupo_producto)]

g11 <- ggplot(tabla_18, aes(x = as.Date(periodo_dt), y = volumen_total, color = grupo_producto)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Fecha",
    y = "Volumen total mensual",
    title = "Serie temporal del volumen total mensual",
    color = "Grupo"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_paper() +
  theme(legend.position = "bottom")

ggsave(
  filename = fs::path(DIR_FIGURES, "g11_serie_volumen_total_mensual.png"),
  plot = g11,
  width = 10,
  height = 5.8,
  dpi = 320,
  bg = "white"
)

# Table 18: overall price and volume statistics. The volume-weighted price uses
# only rows where both the pump price and the volume are observed.
tabla_19 <- data.table(
  Estadistica = c(
    "Observaciones",
    "Estaciones",
    "Volumen total",
    "Precio medio",
    "Precio ponderado por volumen",
    "Mediana del precio",
    "p95 del precio"
  ),
  Valor = c(
    nrow(eess),
    uniqueN(eess$nro_inscripcion, na.rm = TRUE),
    sum(eess$volumen_num, na.rm = TRUE),
    mean(eess$precio_surtidor_num, na.rm = TRUE),
    sum(eess$precio_surtidor_num * eess$volumen_num, na.rm = TRUE) /
      sum(eess$volumen_num[!is.na(eess$precio_surtidor_num) & !is.na(eess$volumen_num)], na.rm = TRUE),
    median(eess$precio_surtidor_num, na.rm = TRUE),
    quantile(eess$precio_surtidor_num, 0.95, na.rm = TRUE, names = FALSE)
  )
)

tabla_19_out <- copy(tabla_19)

tabla_19_out[Estadistica %in% c("Observaciones", "Estaciones"),
             Valor_fmt := fmt_n(as.numeric(Valor))]

tabla_19_out[Estadistica == "Volumen total",
             Valor_fmt := fmt_num(as.numeric(Valor), digits = 3)]

tabla_19_out[!Estadistica %in% c("Observaciones", "Estaciones", "Volumen total"),
             Valor_fmt := fmt_num(as.numeric(Valor), digits = 3)]

tabla_19_out <- tabla_19_out[, .(Estadistica, Valor = Valor_fmt)]

save_tex_table(
  tabla_19_out,
  fs::path(DIR_TABLES, "18_estadisticas_generales_precio_volumen.tex"),
  caption = "Estadísticas generales de precio y volumen.",
  label = "tab:estadisticas_generales_precio_volumen"
)

# Table 19: price and volume statistics by product
tabla_20 <- eess[
  !is.na(producto_clean),
  .(
    observaciones = .N,
    estaciones = uniqueN(nro_inscripcion),
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_medio = mean(precio_surtidor_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE),
    mediana_precio = median(precio_surtidor_num, na.rm = TRUE),
    p95_precio = quantile(precio_surtidor_num, 0.95, na.rm = TRUE, names = FALSE)
  ),
  by = producto_clean
][order(producto_clean)]

tabla_20_out <- copy(tabla_20)
tabla_20_out[, observaciones := fmt_n(observaciones)]
tabla_20_out[, estaciones := fmt_n(estaciones)]
tabla_20_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_20_out[, precio_medio := fmt_num(precio_medio, digits = 3)]
tabla_20_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]
tabla_20_out[, mediana_precio := fmt_num(mediana_precio, digits = 3)]
tabla_20_out[, p95_precio := fmt_num(p95_precio, digits = 3)]

setnames(
  tabla_20_out,
  c("producto_clean", "observaciones", "estaciones", "volumen_total",
    "precio_medio", "precio_ponderado", "mediana_precio", "p95_precio"),
  c("Producto", "Observaciones", "Estaciones", "Volumen_total",
    "Precio_medio", "Precio_ponderado", "Mediana_precio", "p95_precio")
)

save_tex_table(
  tabla_20_out,
  fs::path(DIR_TABLES, "19_estadisticas_por_producto.tex"),
  caption = "Estadísticas de precio y volumen por producto.",
  label = "tab:estadisticas_por_producto"
)

# Table 20: price and volume statistics by province
tabla_21 <- eess[
  !is.na(provincia_clean),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE)
  ),
  by = provincia_clean
][order(provincia_clean)]

tabla_21_out <- copy(tabla_21)
tabla_21_out[, estaciones := fmt_n(estaciones)]
tabla_21_out[, observaciones := fmt_n(observaciones)]
tabla_21_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_21_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]

setnames(
  tabla_21_out,
  c("provincia_clean", "estaciones", "observaciones", "volumen_total", "precio_ponderado"),
  c("Provincia", "Estaciones", "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_table(
  tabla_21_out,
  fs::path(DIR_TABLES, "20_estadisticas_por_provincia.tex"),
  caption = "Estadísticas de precio y volumen por provincia.",
  label = "tab:estadisticas_por_provincia"
)

# Table 21: top 20 brands by number of stations
tabla_22 <- eess[
  !is.na(bandera_clean),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE)
  ),
  by = bandera_clean
][order(-estaciones, bandera_clean)][1:min(20, .N)]

tabla_22_out <- copy(tabla_22)
tabla_22_out[, estaciones := fmt_n(estaciones)]
tabla_22_out[, observaciones := fmt_n(observaciones)]
tabla_22_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_22_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]

setnames(
  tabla_22_out,
  c("bandera_clean", "estaciones", "observaciones", "volumen_total", "precio_ponderado"),
  c("Bandera", "Estaciones", "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_table(
  tabla_22_out,
  fs::path(DIR_TABLES, "21_top20_banderas.tex"),
  caption = "Top 20 banderas por cantidad de estaciones.",
  label = "tab:top20_banderas"
)

# Table 22: top 20 normalized operators by number of stations

# operador_norm normally exists already from the minimal cleaning above
if (!"operador_norm" %in% names(eess)) {
  eess[, operador_norm := norm_txt(operador)]
  eess[operador_norm %in% c("", "na", "n a", "null", "n d", "nd"), operador_norm := NA_character_]
}

tabla_23 <- eess[
  !is.na(operador_norm),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE)
  ),
  by = operador_norm
][order(-estaciones, operador_norm)][1:min(20, .N)]

tabla_23_out <- copy(tabla_23)
tabla_23_out[, estaciones := fmt_n(estaciones)]
tabla_23_out[, observaciones := fmt_n(observaciones)]
tabla_23_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_23_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]

setnames(
  tabla_23_out,
  c("operador_norm", "estaciones", "observaciones", "volumen_total", "precio_ponderado"),
  c("Operador", "Estaciones", "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_table(
  tabla_23_out,
  fs::path(DIR_TABLES, "22_top20_operadores_por_estaciones.tex"),
  caption = "Top 20 operadores normalizados por cantidad de estaciones.",
  label = "tab:top20_operadores_estaciones"
)

# Table 23: top 20 normalized operators by total volume
tabla_24 <- eess[
  !is.na(operador_norm),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE)
  ),
  by = operador_norm
][order(-volumen_total, operador_norm)][1:min(20, .N)]

tabla_24_out <- copy(tabla_24)
tabla_24_out[, estaciones := fmt_n(estaciones)]
tabla_24_out[, observaciones := fmt_n(observaciones)]
tabla_24_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_24_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]

setnames(
  tabla_24_out,
  c("operador_norm", "estaciones", "observaciones", "volumen_total", "precio_ponderado"),
  c("Operador", "Estaciones", "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_table(
  tabla_24_out,
  fs::path(DIR_TABLES, "23_top20_operadores_por_volumen.tex"),
  caption = "Top 20 operadores normalizados por volumen total.",
  label = "tab:top20_operadores_volumen"
)

# Table 24: top 20 normalized operators by volume-weighted price. Operators with
# zero total volume have an undefined weighted price and are dropped.
tabla_25 <- eess[
  !is.na(operador_norm),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = sum(precio_surtidor_num * volumen_num, na.rm = TRUE) /
      sum(volumen_num[!is.na(precio_surtidor_num) & !is.na(volumen_num)], na.rm = TRUE)
  ),
  by = operador_norm
][is.finite(precio_ponderado)][order(-precio_ponderado, operador_norm)][1:min(20, .N)]

tabla_25_out <- copy(tabla_25)
tabla_25_out[, estaciones := fmt_n(estaciones)]
tabla_25_out[, observaciones := fmt_n(observaciones)]
tabla_25_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_25_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]

setnames(
  tabla_25_out,
  c("operador_norm", "estaciones", "observaciones", "volumen_total", "precio_ponderado"),
  c("Operador", "Estaciones", "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_table(
  tabla_25_out,
  fs::path(DIR_TABLES, "24_top20_operadores_por_precio_ponderado.tex"),
  caption = "Top 20 operadores normalizados por precio ponderado.",
  label = "tab:top20_operadores_precio_ponderado"
)

cat("\nDone. Files written in this block:\n")
cat("- 17_volumen_por_grandes_grupos.tex\n")
cat("- g8_hist_volumen_grupos_niveles.png\n")
cat("- g9_hist_log10_volumen_grupos.png\n")
cat("- g10_hist_log1p_volumen_grupos.png\n")
cat("- g11_serie_volumen_total_mensual.png\n")
cat("- 18_estadisticas_generales_precio_volumen.tex\n")
cat("- 19_estadisticas_por_producto.tex\n")
cat("- 20_estadisticas_por_provincia.tex\n")
cat("- 21_top20_banderas.tex\n")
cat("- 22_top20_operadores_por_estaciones.tex\n")
cat("- 23_top20_operadores_por_volumen.tex\n")
cat("- 24_top20_operadores_por_precio_ponderado.tex\n")

# Brands by year (tables 25-28, figures 12-15) ----

# Same as save_tex_table but with longtable, for tables that span several pages
save_tex_longtable <- function(df, file_path, caption = NULL, label = NULL, align = NULL) {
  tex <- knitr::kable(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    escape = TRUE,
    caption = caption,
    label = label,
    align = align
  )
  writeLines(tex, con = file_path)
}

# Volume-weighted mean price over rows with both price and volume; NA when the
# total volume is zero
weighted_mean_safe <- function(price, vol) {
  ok <- !is.na(price) & !is.na(vol)
  denom <- sum(vol[ok], na.rm = TRUE)
  if (denom == 0) return(NA_real_)
  sum(price[ok] * vol[ok], na.rm = TRUE) / denom
}

# Outlet-year-brand combinations. An outlet that changes brand within a year
# counts under both brands, so yearly brand shares can add up to more than one.
boca_anio_bandera <- unique(
  eess[
    !is.na(nro_inscripcion) & !is.na(anio) & !is.na(bandera_clean),
    .(anio, nro_inscripcion, bandera_clean)
  ]
)

# Stations per year, the denominator of the shares
tot_bocas_anio <- boca_anio_bandera[
  , .(total_estaciones_anio = uniqueN(nro_inscripcion)),
  by = anio
]

# Statistics by brand and year
bandera_anio_stats <- eess[
  !is.na(bandera_clean) & !is.na(anio),
  .(
    estaciones = uniqueN(nro_inscripcion),
    observaciones = .N,
    volumen_total = sum(volumen_num, na.rm = TRUE),
    precio_ponderado = weighted_mean_safe(precio_surtidor_num, volumen_num)
  ),
  by = .(anio, bandera_clean)
][order(anio, -estaciones, bandera_clean)]

bandera_anio_stats[tot_bocas_anio, total_estaciones_anio := i.total_estaciones_anio, on = "anio"]
bandera_anio_stats[, share_estaciones := estaciones / total_estaciones_anio]

# Table 25: top 10 brands per year by number of stations
tabla_26 <- copy(bandera_anio_stats)[
  order(anio, -estaciones, bandera_clean)
][
  , rank_estaciones := seq_len(.N),
  by = anio
][rank_estaciones <= 10]

tabla_26_out <- copy(tabla_26)
tabla_26_out[, estaciones := fmt_n(estaciones)]
tabla_26_out[, observaciones := fmt_n(observaciones)]
tabla_26_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_26_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]
tabla_26_out[, share_estaciones := fmt_pct(share_estaciones, digits = 2)]

setcolorder(tabla_26_out,
            c("anio", "rank_estaciones", "bandera_clean", "estaciones",
              "share_estaciones", "observaciones", "volumen_total", "precio_ponderado"))

setnames(
  tabla_26_out,
  c("anio", "rank_estaciones", "bandera_clean", "estaciones", "share_estaciones",
    "observaciones", "volumen_total", "precio_ponderado"),
  c("Año", "Rank", "Bandera", "Estaciones", "Share_estaciones",
    "Observaciones", "Volumen_total", "Precio_ponderado")
)

save_tex_longtable(
  tabla_26_out,
  fs::path(DIR_TABLES, "25_top10_banderas_por_anio_estaciones.tex"),
  caption = "Top 10 banderas por año según cantidad de estaciones.",
  label = "tab:top10_banderas_anio_estaciones"
)

# Table 26: top 10 brands per year by total volume
tabla_27 <- copy(bandera_anio_stats)[
  order(anio, -volumen_total, bandera_clean)
][
  , rank_volumen := seq_len(.N),
  by = anio
][rank_volumen <= 10]

tabla_27_out <- copy(tabla_27)
tabla_27_out[, estaciones := fmt_n(estaciones)]
tabla_27_out[, observaciones := fmt_n(observaciones)]
tabla_27_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_27_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]
tabla_27_out[, share_estaciones := fmt_pct(share_estaciones, digits = 2)]

setcolorder(tabla_27_out,
            c("anio", "rank_volumen", "bandera_clean", "volumen_total",
              "estaciones", "share_estaciones", "observaciones", "precio_ponderado"))

setnames(
  tabla_27_out,
  c("anio", "rank_volumen", "bandera_clean", "volumen_total", "estaciones",
    "share_estaciones", "observaciones", "precio_ponderado"),
  c("Año", "Rank", "Bandera", "Volumen_total", "Estaciones",
    "Share_estaciones", "Observaciones", "Precio_ponderado")
)

save_tex_longtable(
  tabla_27_out,
  fs::path(DIR_TABLES, "26_top10_banderas_por_anio_volumen.tex"),
  caption = "Top 10 banderas por año según volumen total.",
  label = "tab:top10_banderas_anio_volumen"
)

# Table 27: leading brand of each year by number of stations
tabla_28 <- copy(bandera_anio_stats)[
  order(anio, -estaciones, bandera_clean)
][
  , .SD[1],
  by = anio
]

tabla_28_out <- copy(tabla_28)
tabla_28_out[, estaciones := fmt_n(estaciones)]
tabla_28_out[, observaciones := fmt_n(observaciones)]
tabla_28_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_28_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]
tabla_28_out[, share_estaciones := fmt_pct(share_estaciones, digits = 2)]

setnames(
  tabla_28_out,
  c("anio", "bandera_clean", "estaciones", "share_estaciones",
    "observaciones", "volumen_total", "precio_ponderado", "total_estaciones_anio"),
  c("Año", "Bandera_lider", "Estaciones", "Share_estaciones",
    "Observaciones", "Volumen_total", "Precio_ponderado", "Total_estaciones_año")
)

tabla_28_out[, Total_estaciones_año := fmt_n(as.numeric(Total_estaciones_año))]

save_tex_table(
  tabla_28_out,
  fs::path(DIR_TABLES, "27_bandera_lider_por_anio_estaciones.tex"),
  caption = "Bandera líder de cada año según cantidad de estaciones.",
  label = "tab:bandera_lider_anio_estaciones"
)

# Table 28: leading brand of each year by total volume
tabla_29 <- copy(bandera_anio_stats)[
  order(anio, -volumen_total, bandera_clean)
][
  , .SD[1],
  by = anio
]

tabla_29_out <- copy(tabla_29)
tabla_29_out[, estaciones := fmt_n(estaciones)]
tabla_29_out[, observaciones := fmt_n(observaciones)]
tabla_29_out[, volumen_total := fmt_num(volumen_total, digits = 3)]
tabla_29_out[, precio_ponderado := fmt_num(precio_ponderado, digits = 3)]
tabla_29_out[, share_estaciones := fmt_pct(share_estaciones, digits = 2)]

setnames(
  tabla_29_out,
  c("anio", "bandera_clean", "volumen_total", "estaciones",
    "share_estaciones", "observaciones", "precio_ponderado", "total_estaciones_anio"),
  c("Año", "Bandera_lider", "Volumen_total", "Estaciones",
    "Share_estaciones", "Observaciones", "Precio_ponderado", "Total_estaciones_año")
)

tabla_29_out[, Total_estaciones_año := fmt_n(as.numeric(Total_estaciones_año))]

save_tex_table(
  tabla_29_out,
  fs::path(DIR_TABLES, "28_bandera_lider_por_anio_volumen.tex"),
  caption = "Bandera líder de cada año según volumen total.",
  label = "tab:bandera_lider_anio_volumen"
)

# Brands shown in the figures: the 10 with the most stations on average across
# years; the rest are pooled as "Otras"
top_banderas_global <- bandera_anio_stats[
  , .(estaciones_promedio = mean(estaciones, na.rm = TRUE)),
  by = bandera_clean
][order(-estaciones_promedio, bandera_clean)][1:min(10, .N), bandera_clean]

bandera_plot_data <- copy(bandera_anio_stats)
bandera_plot_data[, bandera_plot := fifelse(
  bandera_clean %in% top_banderas_global,
  bandera_clean,
  "Otras"
)]

bandera_plot_data <- bandera_plot_data[
  , .(
    estaciones = sum(estaciones, na.rm = TRUE),
    volumen_total = sum(volumen_total, na.rm = TRUE),
    total_estaciones_anio = max(total_estaciones_anio, na.rm = TRUE)
  ),
  by = .(anio, bandera_plot)
]

bandera_plot_data[, share_estaciones := estaciones / total_estaciones_anio]

# Legend order by average share
order_band <- bandera_plot_data[
  , .(share_promedio = mean(share_estaciones, na.rm = TRUE)),
  by = bandera_plot
][order(-share_promedio)]

bandera_plot_data[, bandera_plot := factor(bandera_plot, levels = order_band$bandera_plot)]

# Figure 12: stations per year for the main brands
g12 <- ggplot(
  bandera_plot_data[bandera_plot != "Otras"],
  aes(x = anio, y = estaciones, color = bandera_plot)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    x = "Año",
    y = "Cantidad de estaciones",
    title = "Evolución anual de estaciones por bandera",
    subtitle = "Principales banderas según presencia promedio"
  ) +
  theme_paper() +
  theme(legend.position = "bottom")

ggsave(
  filename = fs::path(DIR_FIGURES, "g12_evolucion_anual_banderas_estaciones.png"),
  plot = g12,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)

# Figure 13: yearly share of stations by brand
g13 <- ggplot(
  bandera_plot_data,
  aes(x = anio, y = share_estaciones, fill = bandera_plot)
) +
  geom_area(color = "white", linewidth = 0.15, alpha = 0.95) +
  labs(
    x = "Año",
    y = "Share de estaciones",
    title = "Composición anual de estaciones por bandera"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g13_share_anual_banderas_estaciones.png"),
  plot = g13,
  width = 10,
  height = 6.5,
  dpi = 320,
  bg = "white"
)

# Figure 14: top 10 brands by stations in the last year of the panel
ultimo_anio <- max(bandera_anio_stats$anio, na.rm = TRUE)

g14_data <- copy(bandera_anio_stats[anio == ultimo_anio][order(-estaciones, bandera_clean)][1:min(10, .N)])
g14_data[, bandera_clean := factor(bandera_clean, levels = rev(bandera_clean))]

g14 <- ggplot(g14_data, aes(x = bandera_clean, y = estaciones)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Bandera",
    y = "Cantidad de estaciones",
    title = paste0("Top 10 banderas por estaciones en ", ultimo_anio)
  ) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g14_top10_banderas_ultimo_anio_estaciones.png"),
  plot = g14,
  width = 8.5,
  height = 6,
  dpi = 320,
  bg = "white"
)

# Figure 15: top 10 brands by volume in the last year of the panel
g15_data <- copy(bandera_anio_stats[anio == ultimo_anio][order(-volumen_total, bandera_clean)][1:min(10, .N)])
g15_data[, bandera_clean := factor(bandera_clean, levels = rev(bandera_clean))]

g15 <- ggplot(g15_data, aes(x = bandera_clean, y = volumen_total)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Bandera",
    y = "Volumen total",
    title = paste0("Top 10 banderas por volumen en ", ultimo_anio)
  ) +
  theme_paper()

ggsave(
  filename = fs::path(DIR_FIGURES, "g15_top10_banderas_ultimo_anio_volumen.png"),
  plot = g15,
  width = 8.5,
  height = 6,
  dpi = 320,
  bg = "white"
)

cat("\nDone. Files written in this block:\n")
cat("- 25_top10_banderas_por_anio_estaciones.tex\n")
cat("- 26_top10_banderas_por_anio_volumen.tex\n")
cat("- 27_bandera_lider_por_anio_estaciones.tex\n")
cat("- 28_bandera_lider_por_anio_volumen.tex\n")
cat("- g12_evolucion_anual_banderas_estaciones.png\n")
cat("- g13_share_anual_banderas_estaciones.png\n")
cat("- g14_top10_banderas_ultimo_anio_estaciones.png\n")
cat("- g15_top10_banderas_ultimo_anio_volumen.png\n")

# Part 2. Price gap: YPF against the private brands ----

# Robustness checks (figures R1-R4) of figure 1, the price gap between YPF (the
# state-controlled firm) and rival brands.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
#         (boca (outlet) x product x channel x month)
# Output: figR1_benchmark_shell.png, figR2_ladder_blancas.png,
#         figR3_gap_condicional_IC.png, figR4_distribucion_gap.png (Gráficos/)

BASE   <- file.path(DIR_INTERIM, "eess_all_cleaned7_alternative_sinceappearance.rds")
# Figures are written to a local scratch folder and then copied to the synced
# output folder, because the sync client locks files while they are being written.
OUT    <- file.path(tempdir(), "figures")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
DROP   <- file.path(DIR_OUTPUT, "Gráficos")

# Brand groups and regimes ----
# PRIV are the large private brands; BLANCA are the blancas (unbranded,
# independent stations). Focal products: regular gasoline and grade 2 diesel.
PRIV   <- c("SHELL C.A.P.S.A.","ESSO PETROLERA ARGENTINA S.R.L","AXION","PETROBRAS","Pampa Energia","PUMA","OIL COMBUSTIBLES S.A.")
BLANCA <- c("BLANCA","SIN EMPRESA BANDERA")
PRODS  <- c("Nafta (súper) entre 92 y 95 Ron","Gas Oil Grado 2")
# Regime breaks: YPF nationalized (May 2012), price deregulation (November 2017),
# price freeze (August 2019).
REGS   <- data.frame(x=as.Date(c("2012-05-01","2017-11-01","2019-08-01")),
                     lab=c("2012 · YPF estatal","2017 · desregulación","2019 · congelamiento"))

regime_of <- function(d) factor(
  fifelse(d <  as.Date("2012-05-01"), "YPF privada",
   fifelse(d <  as.Date("2017-11-01"), "Estatal·regulado",
    fifelse(d <  as.Date("2019-08-01"), "Estatal·desreg.", "Estatal·congel."))),
  levels=c("YPF privada","Estatal·regulado","Estatal·desreg.","Estatal·congel."))

# Sample ----
# Retail channel, focal products, positive pre-tax price.
b <- readRDS(BASE); setDT(b)
b <- b[canal_de_comercializacion=="Al público"]
b[, precio := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))]
b <- b[!is.na(precio) & precio>0 & producto %in% PRODS]
b[, es_shell := bandera=="SHELL C.A.P.S.A."]
b[, grupo := fifelse(bandera=="YPF","YPF",
              fifelse(bandera %in% PRIV,"Priv",
               fifelse(bandera %in% BLANCA,"Blanca","Otras")))]

# Mean price of each group by cell (locality x month x product)
cm <- b[, .(
  p_ypf   = mean(precio[grupo=="YPF"]),
  p_priv  = mean(precio[grupo=="Priv"]),
  p_shell = mean(precio[es_shell]),
  p_blan  = mean(precio[grupo=="Blanca"])
), by=.(producto, provincia, localidad, periodo_dt)]
cm[, regime := regime_of(periodo_dt)]

# Gaps by cell. Gaps above 40% in absolute value are gross errors and are set
# to missing.
cm[, g_priv  := p_ypf/p_priv  - 1]
cm[, g_shell := p_ypf/p_shell - 1]
cm[, g_blan  := p_blan/p_priv - 1]
for (v in c("g_priv","g_shell","g_blan")) cm[abs(get(v))>0.4, (v):=NA_real_]

# R1. Constant benchmark: Shell alone instead of the large private brands ----
r1 <- rbind(
  cm[!is.na(g_priv),  .(gap=median(g_priv)),  by=.(producto,periodo_dt)][, bench:="vs Privadas grandes"],
  cm[!is.na(g_shell), .(gap=median(g_shell)), by=.(producto,periodo_dt)][, bench:="vs Shell (constante)"])
gR1 <- ggplot(r1, aes(periodo_dt, gap, color=bench)) +
  geom_hline(yintercept=0, color="grey55", linewidth=.3) +
  geom_vline(data=REGS, aes(xintercept=x), linetype="dashed", color="grey45", linewidth=.3) +
  geom_line(linewidth=.6) + facet_wrap(~producto, ncol=1) +
  scale_y_continuous(labels=percent_format(accuracy=1)) + scale_x_date(date_breaks="3 years", date_labels="%Y") +
  scale_color_manual(values=c("vs Privadas grandes"="#1F3864","vs Shell (constante)"="#C55A11")) +
  coord_cartesian(ylim=c(-0.13,0.06)) +
  labs(title="R1 · El gap de YPF es robusto al benchmark",
       subtitle="Mediana entre localidades del gap de precio de YPF. <0 = YPF más barata.",
       x=NULL, y="Gap YPF (%)", color=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"))

# R2. Price ladder: YPF and blancas against the large private brands ----
r2 <- rbind(
  cm[!is.na(g_priv), .(gap=median(g_priv)), by=.(producto,periodo_dt)][, grp:="YPF"],
  cm[!is.na(g_blan), .(gap=median(g_blan)), by=.(producto,periodo_dt)][, grp:="Blancas (independientes)"])
gR2 <- ggplot(r2, aes(periodo_dt, gap, color=grp)) +
  geom_hline(yintercept=0, color="grey55", linewidth=.3) +
  geom_vline(data=REGS, aes(xintercept=x), linetype="dashed", color="grey45", linewidth=.3) +
  geom_line(linewidth=.6) + facet_wrap(~producto, ncol=1) +
  scale_y_continuous(labels=percent_format(accuracy=1)) + scale_x_date(date_breaks="3 years", date_labels="%Y") +
  scale_color_manual(values=c("YPF"="#1F3864","Blancas (independientes)"="#2E7D32")) +
  coord_cartesian(ylim=c(-0.13,0.06)) +
  labs(title="R2 · ¿Quién es más barato? YPF vs. Blancas (ref.: Privadas grandes)",
       subtitle="Mediana entre localidades del gap de precio vs. las privadas grandes. <0 = más barato que las privadas.",
       x=NULL, y="Gap vs. Privadas grandes (%)", color=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"))

# R3. Gap within locality, by regime, with 95% CI clustered by locality ----
# The gap is first averaged within locality, so the standard error of the mean
# across localities treats each locality as one cluster.
r3build <- function(col, lab){
  loc <- cm[!is.na(get(col)), .(g=mean(get(col))), by=.(producto,regime,localidad)]
  loc[, .(est=mean(g), se=sd(g)/sqrt(.N), nloc=.N), by=.(producto,regime)][, grp:=lab]
}
r3 <- rbind(r3build("g_priv","YPF vs Privadas"), r3build("g_blan","Blancas vs Privadas"))
r3[, `:=`(lo=est-1.96*se, hi=est+1.96*se)]
gR3 <- ggplot(r3, aes(regime, est, color=grp)) +
  geom_hline(yintercept=0, color="grey55", linewidth=.3) +
  geom_pointrange(aes(ymin=lo, ymax=hi), position=position_dodge(width=.4), size=.5) +
  facet_wrap(~producto, ncol=1) +
  scale_y_continuous(labels=percent_format(accuracy=.1)) +
  scale_color_manual(values=c("YPF vs Privadas"="#1F3864","Blancas vs Privadas"="#2E7D32")) +
  labs(title="R3 · Gap condicional dentro de localidad (IC 95% clusterizado x localidad)",
       subtitle="Diferencia media de precio vs. privadas grandes, dentro de la misma localidad-mes, por régimen.",
       x=NULL, y="Gap medio (%)", color=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"),
                                       axis.text.x=element_text(angle=20, hjust=1))

# R4. Distribution of the gap across localities (p25, median, p75) ----
r4 <- cm[!is.na(g_priv), .(p25=quantile(g_priv,.25), p50=median(g_priv), p75=quantile(g_priv,.75)),
         by=.(producto,periodo_dt)]
gR4 <- ggplot(r4, aes(periodo_dt)) +
  geom_hline(yintercept=0, color="grey55", linewidth=.3) +
  geom_vline(data=REGS, aes(xintercept=x), linetype="dashed", color="grey45", linewidth=.3) +
  geom_ribbon(aes(ymin=p25, ymax=p75), fill="#1F3864", alpha=.18) +
  geom_line(aes(y=p50), color="#1F3864", linewidth=.6) +
  facet_wrap(~producto, ncol=1) +
  scale_y_continuous(labels=percent_format(accuracy=1)) + scale_x_date(date_breaks="3 years", date_labels="%Y") +
  coord_cartesian(ylim=c(-0.15,0.08)) +
  labs(title="R4 · ¿Es todo el mercado o unas pocas localidades?",
       subtitle="Gap YPF vs. privadas grandes: mediana (línea) y rango intercuartil p25–p75 (banda) entre localidades.",
       x=NULL, y="Gap YPF vs. privadas (%)") +
  theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold"))

# Save figures ----
figs <- list(R1_benchmark_shell=gR1, R2_ladder_blancas=gR2, R3_gap_condicional_IC=gR3, R4_distribucion_gap=gR4)
dims <- list(c(9.5,6), c(9.5,6), c(9,6.2), c(9.5,6))
for (i in seq_along(figs)) {
  fn <- paste0("figR", i, "_", names(figs)[i], ".png")
  ggsave(file.path(OUT, fn), figs[[i]], width=dims[[i]][1], height=dims[[i]][2], dpi=150)
  ok <- file.copy(file.path(OUT, fn), file.path(DROP, fn), overwrite=TRUE)
  cat("saved:", fn, "| copied to output folder:", ok, "\n")
}

# Console diagnostics ----
cat("\nR1: correlation between the gap vs. large private brands and the gap vs. Shell, by product\n")
r1w <- dcast(r1, producto+periodo_dt~bench, value.var="gap")
print(r1w[, .(cor=round(cor(`vs Privadas grandes`,`vs Shell (constante)`, use="complete.obs"),3)), by=producto])
cat("\nR2/R3: mean gap by regime (%), YPF and blancas vs. large private brands\n")
print(r3[, .(producto, regime, grp, est=round(est*100,2), IC=paste0("[",round(lo*100,1),",",round(hi*100,1),"]"), nloc)][order(producto,grp,regime)])
cat("\nR4: % of locality-months where YPF is cheaper (g_priv < 0)\n")
print(cm[!is.na(g_priv), .(pct_ypf_mas_barata=round(mean(g_priv<0)*100,1), celdas=.N), by=.(producto,regime)][order(producto,regime)])
cat("\nFigures R1-R4 done.\n")

# Part 3. Volumes, shares and reporting gaps ----

# Quantities, market shares and reporting gaps in the station panel: reporting
# gaps (Q1), shares in volume vs. outlets (Q2), volume HHI (Q3), outlet size (Q4).
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: tables 29-32 (Tablas/) and figQ1-figQ4 (Gráficos/)

# Paths (same layout as 01_market_structure.R) ----
FILE_BASE   <- fs::path(DIR_INPUT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")

# Figures are written to a local temporary folder and then copied to the synced
# output folder, because the sync client locks files while they are being written.
DIR_TMP <- fs::path(tempdir(), "figs_bloqueQ")
fs::dir_create(DIR_TMP, recurse = TRUE)
fs::dir_create(DIR_TABLES, recurse = TRUE)
fs::dir_create(DIR_FIGURES, recurse = TRUE)
if (!fs::file_exists(FILE_BASE)) stop("Input file with crosswalk not found: ", FILE_BASE)

# Helpers ----
# LaTeX table writer as in 01_market_structure.R. fmt_n() uses a period as
# thousands separator, as in the Spanish tables.
save_tex_table <- function(df, file_path, caption = NULL, label = NULL, align = NULL) {
  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, longtable = FALSE,
                      linesep = "", escape = TRUE, caption = caption,
                      label = label, align = align)
  writeLines(tex, con = file_path)
  cat("  saved:", basename(file_path), "\n")
}
fmt_n <- function(x) format(round(x), big.mark = ".", scientific = FALSE, trim = TRUE)

# Brand groups, products and regimes (same as 02_ypf_private_gap.R) ----
PRIV   <- c("SHELL C.A.P.S.A.", "ESSO PETROLERA ARGENTINA S.R.L", "AXION",
            "PETROBRAS", "Pampa Energia", "PUMA", "OIL COMBUSTIBLES S.A.")
BLANCA <- c("BLANCA", "SIN EMPRESA BANDERA")
PRODS  <- c("Nafta (súper) entre 92 y 95 Ron", "Gas Oil Grado 2")
REGS   <- data.frame(x = as.Date(c("2012-05-01", "2017-11-01", "2019-08-01")),
                     lab = c("2012 · YPF estatal", "2017 · desregulación", "2019 · congelamiento"))
# Pampas core vs. periphery (Pampeana region, as in Culós et al. 2024)
NUCLEO <- c("BUENOS AIRES", "CAPITAL FEDERAL", "SANTA FE", "CORDOBA", "ENTRE RIOS", "LA PAMPA")

regime_of <- function(d) factor(
  fifelse(d <  as.Date("2012-05-01"), "YPF privada",
   fifelse(d <  as.Date("2017-11-01"), "Estatal·regulado",
    fifelse(d <  as.Date("2019-08-01"), "Estatal·desreg.", "Estatal·congel."))),
  levels = c("YPF privada", "Estatal·regulado", "Estatal·desreg.", "Estatal·congel."))

# Sample ----
# Retail channel ("Al público"), the two focal products, positive volume.
b <- readRDS(FILE_BASE); setDT(b)
f <- b[canal_de_comercializacion == "Al público" & producto %in% PRODS]
f[, vol := suppressWarnings(as.numeric(as.character(volumen)))]
f <- f[!is.na(vol) & vol > 0]
f[, grupo := fifelse(bandera == "YPF", "YPF",
              fifelse(bandera %in% PRIV, "Privadas grandes",
               fifelse(bandera %in% BLANCA, "Blancas", "Otras")))]
f[, nucleo := fifelse(provincia %in% NUCLEO, "Núcleo pampeano", "Periferia")]

# Taxed and tax-exempt sales: the 0/1 flag `excentos` splits each cell into a
# taxed and an exempt row. The unit is boca (outlet) x product x month, so
# volumes are summed.
pm <- f[, .(vol = sum(vol), n_filas = .N),
        by = .(nro_inscripcion, producto, periodo_dt, provincia, departamento,
               localidad, bandera, grupo, nucleo, cuit)]
pm[, tt := (year(periodo_dt) - 2004L) * 12L + month(periodo_dt)]
pm[, regime := regime_of(periodo_dt)]
# Market id: province x department, with departments taken from the crosswalk
# (code/03_spatial_variables/crosswalk_locality_department.R). Department names
# repeat across provinces, so counting `departamento` alone undercounts markets.
pm[, mercado := paste0(provincia, "||", departamento)]

cat("Sample\n")
cat("focal rows:", fmt_n(nrow(f)), "| outlet x product x month:", fmt_n(nrow(pm)),
    "(collapsed", nrow(f) - nrow(pm), "taxed/exempt rows)\n")
cat("outlets:", uniqueN(pm$nro_inscripcion), "| markets (province x department):", uniqueN(pm$mercado),
    "| distinct department names:", uniqueN(pm$departamento),
    "| months:", uniqueN(pm$periodo_dt), "\n\n")

# Q1. Reporting gaps in the panel (table 29, figQ1) ----
# A missing outlet-month is a month inside the active span of the outlet (first
# to last appearance) with no row. The data hold no zeros, because cleaned3_cut
# (01_build_panel/02_clean_volume.R) keeps volume >= 1e-3, so "did not report"
# and "sold nothing" cannot be told apart.
sp <- pm[, .(tt0 = min(tt), tt1 = max(tt), nobs = .N), by = .(nro_inscripcion, producto)]
sp[, nmeses := tt1 - tt0 + 1L][, huecos := nmeses - nobs][, pct_hueco := huecos / nmeses]

setorder(pm, nro_inscripcion, producto, tt)
pm[, gap := tt - shift(tt) - 1L, by = .(nro_inscripcion, producto)]
gg <- pm[!is.na(gap) & gap > 0]

.pct_falt  <- 100 * sum(sp$huecos) / sum(sp$nmeses)
.n_flick   <- uniqueN(gg[gap >= 6, .(nro_inscripcion, producto)])
tab29 <- data.table(
  Metrica = c("Series (boca x producto)",
              "Boca-mes observadas",
              "Boca-mes esperadas (dentro del span activo)",
              "Boca-mes FALTANTES",
              "Series con al menos un hueco",
              "Interrupciones de 1-2 meses",
              "Interrupciones de 3-5 meses",
              "Interrupciones de 6+ meses (flickering)",
              "Series con al menos una interrupcion de 6+ meses"),
  Valor = c(fmt_n(nrow(sp)), fmt_n(sum(sp$nobs)), fmt_n(sum(sp$nmeses)),
            fmt_n(sum(sp$huecos)), fmt_n(sum(sp$huecos > 0)),
            fmt_n(sum(gg$gap <= 2)), fmt_n(sum(gg$gap >= 3 & gg$gap <= 5)),
            fmt_n(sum(gg$gap >= 6)), fmt_n(.n_flick)),
  # Plain `%`: kable(escape = TRUE) escapes it, so writing \% here would escape it twice
  Porcentaje = c("",
                 sprintf("%.1f%% de las esperadas", 100 * sum(sp$nobs) / sum(sp$nmeses)),
                 "100%",
                 sprintf("%.1f%% de las esperadas", .pct_falt),
                 sprintf("%.1f%% de las series", 100 * mean(sp$huecos > 0)),
                 "", "", "",
                 sprintf("%.1f%% de las series", 100 * .n_flick / nrow(sp))))

cat("Q1. Reporting gaps\n"); print(tab29)
save_tex_table(tab29, fs::path(DIR_TABLES, "29_huecos_reporte_panel.tex"),
  caption = paste("Huecos de reporte del panel. Un \\emph{faltante} es un mes dentro del",
                  "span activo de la boca (primera--última aparición) sin fila en la base.",
                  "Como la limpieza eliminó los volúmenes nulos, no es posible distinguir",
                  "\\emph{no reportó} de \\emph{vendió cero}.",
                  "Muestra: nafta súper y gasoil grado 2, canal al público, volumen positivo."),
  label = "huecos_panel")

# Figure Q1: outlets observed vs. active (inside their span), by month
act   <- sp[, .(tt = seq.int(tt0, tt1)), by = .(nro_inscripcion, producto)]
q1    <- merge(act[, .(activas = uniqueN(nro_inscripcion)), by = tt],
               pm[, .(observadas = uniqueN(nro_inscripcion)), by = tt], by = "tt", all.x = TRUE)
q1[is.na(observadas), observadas := 0]
q1[, fecha := as.Date(sprintf("%d-%02d-01", 2004L + (tt - 1L) %/% 12L, (tt - 1L) %% 12L + 1L))]
q1[, pct_hueco := 1 - observadas / activas]
cat("\n% of outlet-months missing, by year\n")
print(q1[, .(activas = round(mean(activas)), observadas = round(mean(observadas)),
             pct_hueco = round(100 * mean(pct_hueco), 1)), by = .(anio = year(fecha))][order(anio)])

q1l <- melt(q1[, .(fecha, `Activas (dentro de su span)` = activas, `Observadas (reportan)` = observadas)],
            id.vars = "fecha", variable.name = "serie", value.name = "bocas")
gQ1 <- ggplot(q1l, aes(fecha, bocas, color = serie)) +
  geom_vline(data = REGS, aes(xintercept = x), linetype = "dashed", color = "grey45", linewidth = .3) +
  geom_line(linewidth = .6) +
  scale_color_manual(values = c("Activas (dentro de su span)" = "#C55A11", "Observadas (reportan)" = "#1F3864")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "Q1 · El panel no es balanceado: huecos de reporte",
       subtitle = "Bocas dentro de su span activo (primera–última aparición) vs. bocas que efectivamente reportan.\nLa brecha son boca-mes faltantes. Muestra: nafta súper + gasoil G2, canal al público.",
       x = NULL, y = "Bocas", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(fs::path(DIR_TMP, "figQ1_huecos_panel.png"), gQ1, width = 9, height = 4.6, dpi = 200)

# Volume cap for the quantity analyses (Q2-Q4) ----
# A retail outlet does not sell more than about 3,000 m3 a month (100,000 litres
# a day). Larger values are wholesale or depot deliveries misclassified into the
# "Al público" channel: they recur at fixed locations (Perdriel, Mendoza, next to
# the Luján de Cuyo refinery; the city of Buenos Aires; Posadas) and reach about
# 850,000 m3 a month for a single outlet. The median outlet-month is stable
# (36-91 m3) and there is no change of units, so the problem is only the tail.
# With the cap, national annual volume of the two focal products is 10-17.5
# million m3, in line with actual consumption; without it, 222 million.
#
# The cap is not applied to `pm`: Q1 measures reporting (whether the row exists),
# not magnitudes, and dropping outlet-months by volume would inflate the gaps.
VOL_CAP <- 3000
pmv <- pm[vol <= VOL_CAP]
cat(sprintf("\nVolume cap (Q2-Q4)\ndropped %s of %s outlet-months (%.2f%%) | total volume from %s to %s thousand m3\n",
            fmt_n(nrow(pm) - nrow(pmv)), fmt_n(nrow(pm)),
            100 * (nrow(pm) - nrow(pmv)) / nrow(pm),
            fmt_n(round(sum(pm$vol) / 1e3)), fmt_n(round(sum(pmv$vol) / 1e3))))

# Q2. Market shares: volume vs. number of outlets (table 30, figQ2) ----
nat_vol <- pmv[, .(vol = sum(vol)), by = .(periodo_dt, producto, grupo)]
nat_vol[, share := vol / sum(vol), by = .(periodo_dt, producto)]
nat_boc <- pmv[, .(n = uniqueN(nro_inscripcion)), by = .(periodo_dt, producto, grupo)]
nat_boc[, share := n / sum(n), by = .(periodo_dt, producto)]

cmp <- merge(
  nat_vol[, .(sv = 100 * mean(share)), by = .(regime = regime_of(periodo_dt), producto, grupo)],
  nat_boc[, .(sb = 100 * mean(share)), by = .(regime = regime_of(periodo_dt), producto, grupo)],
  by = c("regime", "producto", "grupo"))
cmp <- cmp[grupo != "Otras"]
setorder(cmp, producto, grupo, regime)
tab30 <- cmp[, .(Producto = producto, Regimen = as.character(regime), Grupo = grupo,
                 `Share en bocas (%)` = round(sb, 1),
                 `Share en volumen (%)` = round(sv, 1),
                 `Ratio vol/bocas` = round(sv / sb, 2))]
cat("\nQ2. Shares: volume vs. outlets\n"); print(tab30)
save_tex_table(tab30, fs::path(DIR_TABLES, "30_shares_volumen_vs_bocas.tex"),
  caption = paste("Participación nacional por grupo de bandera, medida en número de bocas y en",
                  "volumen vendido, por régimen. Un ratio mayor a 1 indica que las bocas del grupo",
                  "son más grandes que el promedio. Promedio de los shares mensuales dentro de cada",
                  "régimen. Muestra: nafta súper y gasoil grado 2, canal al público."),
  label = "shares_vol_bocas")

ypf <- rbind(nat_vol[grupo == "YPF", .(periodo_dt, producto, share, medida = "Share en VOLUMEN")],
             nat_boc[grupo == "YPF", .(periodo_dt, producto, share, medida = "Share en BOCAS")])
gQ2 <- ggplot(ypf, aes(periodo_dt, share, color = medida)) +
  geom_vline(data = REGS, aes(xintercept = x), linetype = "dashed", color = "grey45", linewidth = .3) +
  geom_line(linewidth = .6) + facet_wrap(~producto, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = c("Share en VOLUMEN" = "#1F3864", "Share en BOCAS" = "#7F7F7F")) +
  labs(title = "Q2 · YPF vende más de lo que su red sugiere",
       subtitle = "Participación nacional de YPF. El share en volumen supera al share en bocas ⇒ sus estaciones son más grandes.",
       x = NULL, y = "Share de YPF", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(fs::path(DIR_TMP, "figQ2_share_ypf_volumen_vs_bocas.png"), gQ2, width = 9, height = 6, dpi = 200)

# Q3. Volume HHI by market (table 31, figQ3) ----
# Firm definition: branded stations compete as one brand, while blancas
# (unbranded) are independent, one firm per CUIT (tax id) or per outlet when the
# CUIT is missing. Same convention as the market-definition diagnostics in
# crosswalk_locality_department.R.
pmv[, firma := fifelse(grupo == "Blancas",
                      paste0("BLANCA::", fifelse(is.na(cuit) | trimws(cuit) == "",
                                                 as.character(nro_inscripcion), as.character(cuit))),
                      as.character(bandera))]
hh <- pmv[, .(vol = sum(vol)), by = .(provincia, departamento, nucleo, producto, periodo_dt, firma)]
hh[, s := vol / sum(vol), by = .(provincia, departamento, producto, periodo_dt)]
hhi <- hh[, .(hhi = 10000 * sum(s^2), nfirmas = .N),
          by = .(provincia, departamento, nucleo, producto, periodo_dt)]
hhi[, regime := regime_of(periodo_dt)]

tab31 <- hhi[, .(`HHI mediano` = as.numeric(round(median(hhi))),
                 `Firmas equivalentes` = round(10000 / median(hhi), 2),
                 `Firmas por mercado (mediana)` = as.numeric(median(nfirmas)),
                 `Mercados monopolicos (%)` = round(100 * mean(nfirmas == 1), 1),
                 `Mercados` = uniqueN(paste0(provincia, "||", departamento))),
             by = .(Region = nucleo, Producto = producto)][order(Producto, Region)]
cat("\nQ3. Volume HHI\n"); print(tab31)
save_tex_table(tab31, fs::path(DIR_TABLES, "31_hhi_volumen_nucleo_periferia.tex"),
  caption = paste("Concentración en volumen por mercado (provincia $\\times$ departamento $\\times$ mes).",
                  "HHI $=$ suma de los cuadrados de los \\emph{shares} de volumen, $\\times 10.000$.",
                  "Las \\emph{firmas equivalentes} ($10.000/HHI$) indican a cuántas firmas de igual",
                  "tamaño equivale la concentración observada. Firma $=$ bandera; las blancas se",
                  "cuentan como firmas independientes (por CUIT). Mediana entre mercados y meses."),
  label = "hhi_volumen")

cat("\nMedian HHI by regime\n")
print(dcast(hhi[, .(hhi = as.numeric(round(median(hhi)))), by = .(regime, nucleo, producto)],
            producto + nucleo ~ regime, value.var = "hhi"))

hhi_m <- hhi[, .(hhi = median(hhi)), by = .(nucleo, producto, periodo_dt)]
gQ3 <- ggplot(hhi_m, aes(periodo_dt, hhi, color = nucleo)) +
  geom_hline(yintercept = 2500, linetype = "dotted", color = "grey40") +
  geom_vline(data = REGS, aes(xintercept = x), linetype = "dashed", color = "grey45", linewidth = .3) +
  geom_line(linewidth = .6) + facet_wrap(~producto, ncol = 1) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = c("Núcleo pampeano" = "#1F3864", "Periferia" = "#C55A11")) +
  labs(title = "Q3 · Concentración en volumen: dos mercados distintos",
       subtitle = "HHI mediano entre mercados (provincia × departamento × mes), con shares de VOLUMEN por firma.\nLínea punteada = 2.500 (umbral de alta concentración).",
       x = NULL, y = "HHI mediano", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(fs::path(DIR_TMP, "figQ3_hhi_volumen_nucleo_periferia.png"), gQ3, width = 9, height = 6, dpi = 200)

# Q4. Outlet size: monthly volume per outlet (table 32, figQ4) ----
tab32 <- pmv[grupo != "Otras",
            .(`Boca-mes` = .N, `p25` = round(quantile(vol, .25)), `Mediana` = round(median(vol)),
              `p75` = round(quantile(vol, .75)), `p95` = round(quantile(vol, .95))),
            by = .(Producto = producto, Grupo = grupo)][order(Producto, -Mediana)]
cat("\nQ4. Volume per outlet-month (m3)\n"); print(tab32)
save_tex_table(tab32, fs::path(DIR_TABLES, "32_volumen_mediano_por_boca.tex"),
  caption = paste("Tamaño de la boca: distribución del volumen mensual por estación (m$^3$),",
                  "por grupo de bandera. Muestra: nafta súper y gasoil grado 2, canal al público,",
                  "volumen positivo. Insumo para el tamaño de mercado del modelo de demanda."),
  label = "tamano_boca")

gQ4 <- ggplot(pmv[grupo != "Otras"], aes(x = vol, fill = grupo)) +
  geom_density(alpha = .35, color = NA) +
  scale_x_log10(labels = comma_format(accuracy = 1)) +
  facet_wrap(~producto, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("YPF" = "#1F3864", "Privadas grandes" = "#C55A11", "Blancas" = "#2E7D32")) +
  labs(title = "Q4 · Tamaño de la boca: las estaciones de YPF venden más",
       subtitle = "Densidad del volumen mensual por boca (escala log). Insumo para el tamaño de mercado del BLP.",
       x = "Volumen mensual por boca (m³, escala log)", y = "Densidad", fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(fs::path(DIR_TMP, "figQ4_tamano_boca.png"), gQ4, width = 9, height = 6, dpi = 200)

# Copy figures to the output folder ----
figs <- c("figQ1_huecos_panel.png", "figQ2_share_ypf_volumen_vs_bocas.png",
          "figQ3_hhi_volumen_nucleo_periferia.png", "figQ4_tamano_boca.png")
ok <- file.copy(fs::path(DIR_TMP, figs), fs::path(DIR_FIGURES, figs), overwrite = TRUE)
cat("\nOutput\n")
cat("tables 29-32 written to", as.character(DIR_TABLES), "\n")
cat("figures copied:", sum(ok), "/", length(figs), "to", as.character(DIR_FIGURES), "\n")

# Part 4. Station characteristics ----

# Inventory of observable station characteristics, the X of the BLP demand model:
# variation within and between markets (C1, C2) and highway vs. urban location (C3).
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: tables 33-34 (Tablas/), figC1 and figC2 (Gráficos/)

# Paths (same layout as 01_market_structure.R) ----
FILE_BASE   <- fs::path(DIR_INPUT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
# Figures go to a local temporary folder first and are copied to the synced
# output folder at the end, because the sync client locks files being written.
DIR_TMP <- fs::path(tempdir(), "figs_bloqueC"); fs::dir_create(DIR_TMP, recurse = TRUE)
fs::dir_create(DIR_TABLES, recurse = TRUE); fs::dir_create(DIR_FIGURES, recurse = TRUE)
if (!fs::file_exists(FILE_BASE)) stop("Input file with crosswalk not found: ", FILE_BASE)

save_tex_table <- function(df, file_path, caption = NULL, label = NULL, align = NULL) {
  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, longtable = FALSE,
                      linesep = "", escape = TRUE, caption = caption, label = label, align = align)
  writeLines(tex, con = file_path); cat("  saved:", basename(file_path), "\n")
}

# Products, brand groups and text patterns ----
# Same products, brand groups and core provinces as 02_ypf_private_gap.R and
# 03_quantities_shares.R.
PRODS <- c("Nafta (súper) entre 92 y 95 Ron","Gas Oil Grado 2")
PRIV  <- c("SHELL C.A.P.S.A.","ESSO PETROLERA ARGENTINA S.R.L","AXION","PETROBRAS","Pampa Energia","PUMA","OIL COMBUSTIBLES S.A.")
BLANCA<- c("BLANCA","SIN EMPRESA BANDERA")
NUCLEO<- c("BUENOS AIRES","CAPITAL FEDERAL","SANTA FE","CORDOBA","ENTRE RIOS","LA PAMPA")
# Operator names that mark a company-operated station (as opposed to a dealer)
CO_PAT   <- "OPESSA|AXION ENERGY|PAN AMERICAN ENERGY|EG3|TRAFIGURA|PAMPA ENERGIA|ESSO PETROLERA"
# Highway stations are identified from the address text. Only unambiguous tokens
# are used; ACCESO is left out.
RUTA_PAT <- "\\bRUTA\\b|\\bRN\\b|\\bRP\\b|\\bKM\\b|\\bAUTOPISTA\\b|\\bAUTOVIA\\b"

# Sample ----
# Retail channel, focal products, positive volume. A market is province x
# department x month.
b <- readRDS(FILE_BASE); setDT(b)
f <- b[canal_de_comercializacion=="Al público" & producto %in% PRODS]
f[, vol := suppressWarnings(as.numeric(as.character(volumen)))]
f[, precio := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))]
f <- f[!is.na(vol) & vol>0]
f[, mercado := paste0(provincia,"||",departamento)]
f[, dual := grepl("Duales", tipo_negocio_h_since)]
f[, co   := grepl(CO_PAT, toupper(operador))]
f[, ruta := grepl(RUTA_PAT, toupper(iconv(as.character(direccion),"","ASCII//TRANSLIT")))]
f[, grupo := fifelse(bandera=="YPF","YPF", fifelse(bandera %in% PRIV,"Privadas grandes",
             fifelse(bandera %in% BLANCA,"Blancas","Otras")))]
f[, region := fifelse(provincia %in% NUCLEO,"Núcleo","Periferia")]
# One row per boca (outlet) x market x month; nboc counts the outlets in the
# market-month.
bm <- f[, .(vol=sum(vol), bandera=bandera[1], grupo=grupo[1], region=region[1],
            dual=any(dual), co=any(co), ruta=any(ruta)), by=.(nro_inscripcion, mercado, periodo_dt)]
bm[, mm := paste0(mercado,"||",periodo_dt)][, nboc := uniqueN(nro_inscripcion), by=mm]

# C1. Inventory of characteristics (table 33) ----
# A characteristic varies in the market when it takes two or more values in a
# market-month with at least two outlets. Only then is its taste coefficient
# identified from substitution within the market.
mk2 <- bm[nboc>=2, .(v_bandera=uniqueN(bandera)>=2, v_dual=uniqueN(dual)>=2,
                     v_co=uniqueN(co)>=2, v_ruta=uniqueN(ruta)>=2), by=mm]
pm_ <- function(col) sprintf("%.1f\\%%", 100*mean(mk2[[col]]))
tab33 <- data.table(
  `Característica` = c("Bandera (marca)","Ubicación ruta/urbano","Dual GNC (ofrece GNC)",
                      "Tamaño de boca","Company-op vs dealer","Grado (súper/premium/…)",
                      "Antigüedad (span)","Amenities (surtidores, shop, 24h)","Impuestos locales"),
  `Cobertura` = c("100\\%","100\\%","100\\%","100\\%","100\\%","100\\%","100\\%",
                  "no está","sólo 2024, <8\\%"),
  `Varía en el mercado` = c(pm_("v_bandera"), pm_("v_ruta"), pm_("v_dual"),
                           "continua (siempre)", pm_("v_co"), "dentro de la estación",
                           "casi siempre", "—", "—"),
  `Rol en el modelo` = c("x (coef. aleatorio)","x","x","x / tamaño de mercado",
                        "oferta/costo (no demanda)","caract. de producto","proxy de arraigo",
                        "efecto fijo de estación","inutilizable en panel"))
cat("C1. Table 33\n"); print(tab33)
save_tex_table(tab33, fs::path(DIR_TABLES, "33_inventario_caracteristicas.tex"),
  caption = paste("Inventario de características observables (la ``X'' del modelo de demanda).",
                  "``Varía en el mercado'' $=$ \\% de mercados (depto $\\times$ mes con $\\geq$2 bocas)",
                  "donde la característica toma $\\geq$2 valores: sólo entonces su coeficiente de gusto",
                  "se identifica por sustitución dentro del mercado. Muestra focal."),
  label = "inventario", align = "llll")

# C2. Within/between decomposition (figC1) ----
bm[, `:=`(is_ypf=as.numeric(bandera=="YPF"), is_blanca=as.numeric(bandera %in% BLANCA), lvol=log(vol))]
d <- bm[nboc>=2]
# Share of the variance of x that is within groups g
within_share <- function(x,g){ dt<-data.table(x=as.numeric(x),g=g); gm<-dt[,.(m=mean(x),n=.N),by=g]
  gr<-mean(dt$x); 1 - sum(gm$n*(gm$m-gr)^2)/sum((dt$x-gr)^2) }
vars <- c(`Marca: es YPF`="is_ypf",`Marca: es blanca`="is_blanca",`Tamaño (log volumen)`="lvol",
          `Ubicación ruta/urbano`="ruta",`Dual GNC`="dual")
res <- rbindlist(lapply(names(vars), function(nm)
  data.table(caracteristica=nm, within=within_share(d[[vars[nm]]], d$mm))))
res[, between:=1-within]; setorder(res,-within)
cat("\nC2. Within/between decomposition\n")
print(res[, .(caracteristica, within=round(100*within,1))])
resl <- melt(res, id.vars="caracteristica", measure.vars=c("within","between"),
             variable.name="parte", value.name="frac")
resl[, parte:=factor(parte, levels=c("between","within"),
     labels=c("Entre mercados (va al efecto fijo)","Dentro del mercado (identifica gustos)"))]
resl[, caracteristica:=factor(caracteristica, levels=rev(res$caracteristica))]
gC1 <- ggplot(resl, aes(frac, caracteristica, fill=parte)) +
  geom_col(width=.66) + geom_vline(xintercept=.5, color="grey55", linetype="dotted", linewidth=.3) +
  scale_x_continuous(labels=percent_format(accuracy=1), expand=expansion(0)) +
  scale_fill_manual(values=c("Dentro del mercado (identifica gustos)"="#2E7D32",
                             "Entre mercados (va al efecto fijo)"="#BFBFBF")) +
  labs(title="C2 · Qué característica identifica gustos y cuál se va al efecto fijo",
       subtitle="Variación de cada característica dentro vs. entre mercados (depto × mes, ≥2 bocas).\nMás verde = más variación DENTRO del mercado → coeficiente de gusto mejor identificado.",
       x="Fracción de la variación", y=NULL, fill=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"),
                                       panel.grid.major.y=element_blank())
ggsave(fs::path(DIR_TMP,"figC1_within_between.png"), gC1, width=9, height=4.4, dpi=200)

# C3. Highway vs. urban location (figC2, table 34) ----
boca <- f[, .(ruta=any(ruta), grupo=grupo[1], region=region[1]), by=nro_inscripcion]
tabR <- boca[grupo!="Otras", .(pct=round(100*mean(ruta),1)), by=.(grupo, region)]
tab34 <- dcast(tabR, grupo ~ region, value.var="pct")
setnames(tab34, "grupo", "Grupo")
setcolorder(tab34, c("Grupo","Núcleo","Periferia"))
tab34 <- tab34[match(c("YPF","Privadas grandes","Blancas"), Grupo)]
cat("\nC3. Table 34 (highway location)\n"); print(tab34)
# Price gap between highway and urban stations, in locality x product x month
# cells that have both. Gaps of 40% or more in absolute value are dropped.
fp <- f[!is.na(precio) & precio>0]
cell <- fp[, .(p_ruta=mean(precio[ruta]), p_urb=mean(precio[!ruta]), n_r=sum(ruta), n_u=sum(!ruta)),
           by=.(provincia, localidad, producto, periodo_dt, region)][n_r>0 & n_u>0]
cell[, gap:=p_ruta/p_urb-1]; cell <- cell[abs(gap)<0.4]
gap_med <- 100*median(cell$gap)
cat(sprintf("price gap, highway vs. urban: median %+.2f%% (n=%d cells)\n", gap_med, nrow(cell)))
save_tex_table(tab34, fs::path(DIR_TABLES, "34_ruta_por_grupo_region.tex"),
  caption = paste("\\% de estaciones cuya dirección es de ruta/autopista (vs. urbana),",
                  "por grupo de bandera y región. Clasificación por tokens inequívocos de ruta",
                  "(RUTA/RN/RP/KM/AUTOPISTA) sobre la dirección. Muestra focal."),
  label = "ruta", align = "lrr")

gC2 <- ggplot(tabR, aes(grupo, pct/100, fill=region)) +
  geom_col(position=position_dodge(.7), width=.62) +
  geom_text(aes(label=paste0(pct,"%")), position=position_dodge(.7), vjust=-.35, size=3) +
  scale_y_continuous(labels=percent_format(accuracy=1), expand=expansion(mult=c(0,.12))) +
  scale_fill_manual(values=c("Núcleo"="#1F3864","Periferia"="#C55A11")) +
  labs(title="C3 · Las estaciones de ruta son un fenómeno de la periferia",
       subtitle="% de estaciones en dirección de ruta/autopista, por grupo y región. En la periferia las lideran las blancas;\nel precio de ruta vs. urbano es prácticamente igual (gap ≈ 0) ⇒ es característica de clientela, no de precio.",
       x=NULL, y="% de estaciones de ruta", fill=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"))
ggsave(fs::path(DIR_TMP,"figC2_ruta_urbano.png"), gC2, width=9, height=4.8, dpi=200)

ok <- file.copy(fs::path(DIR_TMP, c("figC1_within_between.png","figC2_ruta_urbano.png")),
                fs::path(DIR_FIGURES, c("figC1_within_between.png","figC2_ruta_urbano.png")), overwrite=TRUE)
cat("\nOutput\ntables 33-34 written to", as.character(DIR_TABLES),
    "\nfigures figC1/figC2 copied:", sum(ok), "/2 to", as.character(DIR_FIGURES), "\n")

# Part 5. Pump price against crude and import parity ----

# National median pre-tax price of regular gasoline (nafta súper) in USD per litre
# against Brent, the US Gulf Coast FOB gasoline price and import parity.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds;
#         covar_series_nacionales.csv, covar_sesco_comercio_ext.csv and
#         raw_series/{usgc_gasolina_fob_fred, dolar_blue_mensual_ambito}.csv
#         in the market covariates folder
# Output: figK1-figK3 (at the parallel and at the official exchange rate), figK4
#         and figK1b (Gráficos/); serie_super_vs_costos.csv

# The pump price is the national median across retail outlets ("Al público") of
# the price without taxes, the supply-side price comparable with costs. It is
# converted at the parallel exchange rate (blue, monthly average from Ámbito) so
# that the exchange controls of 2012-15 and 2019-23 do not inflate the dollar
# price. The same figures at the official A3500 rate, the relevant one for an
# importer's cost, are saved with the suffix "_tc_oficial".
#
# References in USD per litre: Brent / 158.987; FOB / 3.78541, the EIA spot price
# of conventional regular gasoline, US Gulf Coast (USGC), from FRED (id
# MGASUSGULF, USD per gallon); import parity / 1000, the unit value (USD per m3)
# of SESCO imports of Nafta Grado 2 (Súper) and Grado 3 (Ultra), weighted by m3.
# Parity is observed in 2010-24. Months without imports and 2004-09 are projected
# from ln(parity) = a + b * ln(FOB), fitted on the observed months.

pdf(NULL)  # keeps ggplotGrob() from leaving an Rplots.pdf in the working directory

# Figures are saved in a local temporary folder and copied to the synced output
# folder right away, because the sync client locks files while they are written.
DIR_COV   <- DIR_COVAR
DIR_FIG   <- fs::path(DIR_OUTPUT, "Gráficos")
DIR_TMP   <- fs::path(tempdir(), "figs_bloqueK"); fs::dir_create(DIR_TMP)
FILE_BASE <- fs::path(DIR_INPUT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")

# Regime labels and colors as in 02_ypf_private_gap.R to 04_station_characteristics.R
PROD_SUPER <- "Nafta (súper) entre 92 y 95 Ron"
REGS <- data.frame(x=as.Date(c("2012-05-01","2017-10-01","2019-08-01")),
                   lab=c("2012 · YPF estatal","2017 · desregulación","2019 · congelamiento"))
regime_of <- function(d) factor(
  fifelse(d <  as.Date("2012-05-01"), "YPF privada",
   fifelse(d <  as.Date("2017-11-01"), "Estatal·regulado",
    fifelse(d <  as.Date("2019-08-01"), "Estatal·desreg.", "Estatal·congel."))),
  levels=c("YPF privada","Estatal·regulado","Estatal·desreg.","Estatal·congel."))
L_BBL <- 158.987; L_GAL <- 3.78541

# 5.1 Monthly price of regular gasoline (national median, retail channel) ----
b <- readRDS(FILE_BASE); setDT(b)
s <- b[canal_de_comercializacion=="Al público" & producto==PROD_SUPER]
rm(b); invisible(gc())
s[, p_sin := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))]
s[, p_con := suppressWarnings(as.numeric(as.character(precio_con_impuestos)))]
s <- s[!is.na(p_sin) & p_sin>0]
ps <- s[, .(p_sin_ars=median(p_sin), p_con_ars=median(p_con[!is.na(p_con) & p_con>0]), bocas=uniqueN(nro_inscripcion)),
        by=.(mes=as.Date(periodo_dt))][order(mes)]
cat("Regular gasoline price:", nrow(ps), "months,", format(min(ps$mes)), "-", format(max(ps$mes)), "\n")

# 5.2 National series and FOB USGC ----
sn <- fread(fs::path(DIR_COV, "covar_series_nacionales.csv"))
sn[, mes := as.Date(mes)]
fob <- fread(fs::path(DIR_COV, "raw_series/usgc_gasolina_fob_fred.csv"))
setnames(fob, c("mes","fob_usd_gal")); fob[, mes := as.Date(mes)]
# Parallel exchange rate (blue, selling rate, monthly average; Ámbito, 2004-2024).
# Before 2011 it coincides with the official rate.
blue <- fread(fs::path(DIR_COV, "raw_series/dolar_blue_mensual_ambito.csv"))[, .(mes=as.Date(mes), tc_blue=blue_venta_prom)]

# 5.3 Observed import parity (SESCO imports of súper and ultra gasoline) ----
ce <- fread(fs::path(DIR_COV, "covar_sesco_comercio_ext.csv"))
par_obs <- ce[tipo=="Importación" & producto %in% c("Nafta Grado 2 (Súper)(m3)","Nafta Grado 3 (Ultra)(m3)") &
              cantidad>0 & monto_usd>0,
              .(paridad_usd_m3 = sum(monto_usd)/sum(cantidad), m3_impo=sum(cantidad)),
              by=.(mes=as.Date(sprintf("%d-%02d-01", anio, mes)))]
# Unit values outside [200, 2000] USD per m3 are data-entry errors and are dropped
par_obs <- par_obs[paridad_usd_m3 >= 200 & paridad_usd_m3 <= 2000]
cat("Observed parity:", nrow(par_obs), "months between", format(min(par_obs$mes)), "and", format(max(par_obs$mes)), "\n")

# 5.4 Monthly panel ----
d <- merge(ps, sn[, .(mes, tc=tc_a3500_prom, brent_usd_bbl, ipc_2004_100)], by="mes", all.x=TRUE)
d <- merge(d, fob, by="mes", all.x=TRUE)
d <- merge(d, par_obs, by="mes", all.x=TRUE)
d <- merge(d, blue, by="mes", all.x=TRUE)
stopifnot(!anyNA(d$tc), !anyNA(d$tc_blue), !anyNA(d$brent_usd_bbl), !anyNA(d$fob_usd_gal))
d[, `:=`(super_usd_of = p_sin_ars/tc,      super_con_usd_of = p_con_ars/tc,        # official A3500 rate
         super_usd_bl = p_sin_ars/tc_blue, super_con_usd_bl = p_con_ars/tc_blue,   # parallel rate
         brent_usd_l = brent_usd_bbl/L_BBL, fob_usd_l = fob_usd_gal/L_GAL,
         paridad_obs_usd_l = paridad_usd_m3/1000)]
d[, super_usd_l := super_usd_bl]   # main conversion is the parallel rate; the figure loop switches it

# Projected parity: ln(parity) on ln(FOB), fitted on the observed months. The
# fit on Brent is only printed for comparison.
fit_fob   <- lm(log(paridad_obs_usd_l) ~ log(fob_usd_l),   data=d[!is.na(paridad_obs_usd_l)])
fit_brent <- lm(log(paridad_obs_usd_l) ~ log(brent_usd_l), data=d[!is.na(paridad_obs_usd_l)])
cat("\nParity fit (observed months, n =", sum(!is.na(d$paridad_obs_usd_l)), ")\n")
cat("  on FOB USGC : b =", round(coef(fit_fob)[2],3),   " R2 =", round(summary(fit_fob)$r.squared,3), "\n")
cat("  on Brent    : b =", round(coef(fit_brent)[2],3), " R2 =", round(summary(fit_brent)$r.squared,3), "\n")
d[, paridad_pred_usd_l := exp(predict(fit_fob, newdata=d))]
d[, paridad_usd_l := fifelse(is.na(paridad_obs_usd_l), paridad_pred_usd_l, paridad_obs_usd_l)]
d[, paridad_fuente := fifelse(is.na(paridad_obs_usd_l), "proyectada", "observada")]
d[, regime := regime_of(mes)]

# 5.5 Figures K1-K3 ----
COL_SUPER <- "#1F3864"; COL_REF <- "#C55A11"
LAB_SUPER <- "Súper sin impuestos (mediana nacional)"
theme_k <- theme_minimal(base_size=11) + theme(legend.position="bottom", plot.title=element_text(face="bold"),
                                                strip.text=element_text(face="bold"))
# Two stacked panels (levels on top, ratio or gap below) with different y axes.
# The grobs are stacked with gtable, which ggplot2 already depends on, after
# equalizing their widths, so patchwork is not needed.
stack2 <- function(top, bottom, heights=c(1.15, 1)) {
  g1 <- ggplotGrob(top); g2 <- ggplotGrob(bottom)
  w <- grid::unit.pmax(g1$widths, g2$widths); g1$widths <- w; g2$widths <- w
  p1 <- g1$layout[g1$layout$name=="panel", "t"]; g1$heights[p1] <- grid::unit(heights[1], "null")
  p2 <- g2$layout[g2$layout$name=="panel", "t"]; g2$heights[p2] <- grid::unit(heights[2], "null")
  rbind(g1, g2, size="first")
}
save_stack <- function(g, path, width=10, height=7.5, dpi=150) {
  png(path, width=width, height=height, units="in", res=dpi); grid::grid.draw(g); dev.off()
}
xdate <- scale_x_date(date_breaks="2 years", date_labels="%Y")
vregs <- geom_vline(data=REGS, aes(xintercept=x), linetype="dashed", color="grey45", linewidth=.3)

mk_fig <- function(ref_col, ref_lab, title, subtitle, ratio_lab, ratio_is_gap=TRUE, pts=NULL) {
  lv <- rbind(d[, .(mes, serie=LAB_SUPER, y=super_usd_l)],
              d[, .(mes, serie=ref_lab, y=get(ref_col))])
  rt <- d[, .(mes, y = if (ratio_is_gap) super_usd_l/get(ref_col)-1 else super_usd_l/get(ref_col))]
  top <- ggplot() + vregs + geom_line(data=lv, aes(mes, y, color=serie), linewidth=.6)
  if (!is.null(pts)) top <- top + geom_point(data=pts, aes(mes, y), color=COL_REF, size=1.1, alpha=.85)
  top <- top + xdate +
    scale_y_continuous(labels=label_number(accuracy=.01)) +
    scale_color_manual(values=setNames(c(COL_SUPER, COL_REF), c(LAB_SUPER, ref_lab))) +
    labs(title=title, subtitle=subtitle, x=NULL, y="USD por litro", color=NULL) +
    theme_k + theme(legend.position="top", axis.text.x=element_blank())
  bot <- ggplot() + vregs
  if (ratio_is_gap) bot <- bot + geom_hline(yintercept=0, color="grey55", linewidth=.3)
  bot <- bot + geom_line(data=rt, aes(mes, y), color="grey20", linewidth=.6) + xdate +
    labs(x=NULL, y=ratio_lab) + theme_k
  bot <- bot + (if (ratio_is_gap) scale_y_continuous(labels=percent_format(accuracy=1))
                else scale_y_continuous(labels=label_number(accuracy=.1)))
  stack2(top, bot)
}
# Each figure is drawn twice, at the parallel and at the official exchange rate
CONV <- list(paralelo = list(col="super_usd_bl", suf="",            tc_lab="dólar paralelo (blue)"),
             oficial  = list(col="super_usd_of", suf="_tc_oficial", tc_lab="TC oficial A3500"))
for (cv in names(CONV)) {
  d[, super_usd_l := get(CONV[[cv]]$col)]
  SUB <- paste0("Precio sin impuestos de la nafta súper, mediana entre bocas al público, en USD/litro (", CONV[[cv]]$tc_lab, ").",
                "\nLíneas punteadas: 2012 estatización de YPF, 2017 desregulación, 2019 congelamiento.")
  gK1 <- mk_fig("brent_usd_l", "Brent (USD por litro de crudo)",
                "K1 · Nafta súper vs. Brent",
                paste0(SUB, " Abajo: cociente súper / Brent, por litro."),
                "Súper / Brent (veces)", ratio_is_gap=FALSE)
  gK2 <- mk_fig("fob_usd_l", "FOB nafta regular, Golfo de EE.UU. (EIA)",
                "K2 · Nafta súper vs. FOB internacional de nafta",
                paste0(SUB, " Abajo: brecha del precio local sobre el FOB."),
                "Brecha súper sobre FOB (%)", ratio_is_gap=TRUE)
  gK3 <- mk_fig("paridad_usd_l", "Paridad de importación (puntos = observada; línea = observada + proyectada)",
                "K3 · Nafta súper vs. paridad de importación",
                paste0(SUB, "\nParidad = valor unitario de la importación de nafta súper/ultra (SESCO, 2010-24); meses sin importación y 2004-09\nproyectados con el FOB USGC. Abajo: brecha del precio local sobre la paridad."),
                "Brecha súper sobre paridad (%)", ratio_is_gap=TRUE,
                pts=d[paridad_fuente=="observada", .(mes, y=paridad_obs_usd_l)])
  figs <- list(figK1_super_vs_brent=gK1, figK2_super_vs_fob=gK2, figK3_super_vs_paridad=gK3)
  for (nm in names(figs)) {
    fn <- paste0(nm, CONV[[cv]]$suf, ".png")
    save_stack(figs[[nm]], fs::path(DIR_TMP, fn))
    ok <- file.copy(fs::path(DIR_TMP, fn), fs::path(DIR_FIG, fn), overwrite=TRUE)
    cat("saved:", fn, "| copied to output folder:", ok, "\n")
  }
}
d[, super_usd_l := super_usd_bl]

# 5b. K4: the gap over import parity alone, with the policy episodes shaded ----
# Episodes follow the policy toward crude oil and pump prices, not the sign of
# the gap: sliding-scale export duties (Res. 532/2004); barril criollo (regulated
# domestic crude price) as a ceiling (Res. 394/2007, cutoff 42; Res. 1/2013,
# cutoff 70); support price, the barril criollo as a floor (December 2014 to 22
# September 2017); free prices (October 2017 to July 2019); price freezes,
# exchange controls and a de facto barril criollo (DNU 566/2019, Decree 488/2020,
# Precios Justos); liberalization (December 2023).
EPIS <- data.table(
  ini = as.Date(c("2004-12-01","2007-11-01","2015-01-01","2017-10-01","2019-08-01","2023-12-01")),
  fin = as.Date(c("2007-10-31","2014-12-31","2017-09-30","2019-07-31","2023-11-30","2024-12-31")),
  lab = c("2005-07\nRetenciones\nescala móvil",
          "nov-2007 a 2014\nBarril criollo, fase techo\ncrudo interno limitado (42, luego 70)",
          "2015 a sep-2017\nPrecio sostén\ncrudo interno a 77-55",
          "oct-2017 a\njul-2019\nPrecios libres",
          "ago-2019 a nov-2023\nCongelamientos, cepo,\nbarril criollo de hecho",
          "2024\nLiberación"),
  signo = c("neg","neg","pos","cero","neg","cero"))
EPIS[, x := ini + (fin - ini)/2]
gap <- d[, .(mes, brecha = super_usd_bl/paridad_usd_l - 1)]
ytop <- 1.5
gK4 <- ggplot() +
  geom_rect(data=EPIS, aes(xmin=ini, xmax=fin, ymin=-Inf, ymax=Inf, fill=signo), alpha=.18, show.legend=FALSE) +
  scale_fill_manual(values=c(neg=COL_SUPER, pos=COL_REF, cero="grey60")) +
  geom_hline(yintercept=0, color="grey40", linewidth=.35) +
  geom_line(data=gap, aes(mes, brecha), color="grey15", linewidth=.65) +
  geom_text(data=EPIS, aes(x=x, y=ytop, label=lab), size=2.8, lineheight=.95, vjust=1, color="grey20") +
  scale_y_continuous(labels=percent_format(accuracy=1), breaks=seq(-.5, 1.25, .25)) +
  coord_cartesian(ylim=c(-.65, ytop)) + xdate +
  labs(title="K4 · La brecha del precio local sobre la paridad de importación, 2004-2024",
       subtitle="Nafta súper sin impuestos (mediana nacional, dólar paralelo) / paridad de importación − 1.\nTramos según la política sobre el crudo y el surtidor.\nSombreado azul: precio por debajo de la paridad; naranja: por encima; gris: sin intervención.",
       x=NULL, y="Brecha súper sobre paridad") +
  theme_k
ggsave(fs::path(DIR_TMP, "figK4_brecha_episodios.png"), gK4, width=10, height=5.6, dpi=150)
ok <- file.copy(fs::path(DIR_TMP, "figK4_brecha_episodios.png"), fs::path(DIR_FIG, "figK4_brecha_episodios.png"), overwrite=TRUE)
cat("saved: figK4_brecha_episodios.png | copied to output folder:", ok, "\n")
cat("\nGap over parity by episode (median, min, max; parallel rate)\n")
gap[, epi := EPIS$lab[findInterval(mes, EPIS$ini)]]
print(gap[, .(meses=.N, mediana=percent(median(brecha),1), min=percent(min(brecha),1), max=percent(max(brecha),1)), by=.(epi=substr(gsub("\n"," · ",epi),1,40))])

# 5c. K1b: regular gasoline vs. Brent in current pesos (log scale) ----
# Pump price in current pesos against Brent converted to pesos per litre at the
# official and at the parallel rate. On a log scale, parallel lines grow at the
# same rate; the two Brent lines coincide outside the exchange-control periods.
# Bottom panel: ratio of the pump price to Brent in pesos, which equals the ratio
# in USD because the exchange rate cancels out, with reference lines at 1.3
# (market value: crude plus refining and freight) and at 1.
d[, `:=`(brent_ars_of = brent_usd_l * tc, brent_ars_bl = brent_usd_l * tc_blue)]
lv <- rbind(d[, .(mes, serie = "Súper sin impuestos (mediana nacional)", y = p_sin_ars)],
            d[, .(mes, serie = "Brent en pesos, TC oficial", y = brent_ars_of)],
            d[, .(mes, serie = "Brent en pesos, dólar paralelo", y = brent_ars_bl)])
lv[, serie := factor(serie, levels = c("Súper sin impuestos (mediana nacional)", "Brent en pesos, TC oficial", "Brent en pesos, dólar paralelo"))]
rt <- rbind(d[, .(mes, serie = "TC oficial", y = p_sin_ars / brent_ars_of)],
            d[, .(mes, serie = "dólar paralelo", y = p_sin_ars / brent_ars_bl)])
top <- ggplot() + vregs +
  geom_line(data = lv, aes(mes, y, color = serie, linetype = serie), linewidth = .6) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500), labels = label_number(accuracy = 1)) +
  scale_color_manual(values = c(COL_SUPER, COL_REF, COL_REF)) +
  scale_linetype_manual(values = c("solid", "solid", "22")) + xdate +
  labs(title = "K1b · Nafta súper vs. Brent, en pesos corrientes por litro (escala log)",
       subtitle = paste0("Súper sin impuestos, mediana entre bocas al público, en pesos corrientes. Brent pasado a pesos por litro con el TC oficial (llena)",
                         "\ny con el dólar paralelo (punteada). En escala log, líneas paralelas suben al mismo ritmo.",
                         "\nVerticales: 2012 estatización de YPF, 2017 desregulación, 2019 congelamiento. Abajo: cociente súper / Brent, igual en pesos",
                         "\nque en dólares. 1,3 = valor de mercado (crudo más refinación y flete); 1 = la nafta vale lo mismo que el crudo."),
       x = NULL, y = "Pesos por litro (log)", color = NULL, linetype = NULL) +
  theme_k + theme(legend.position = "top", axis.text.x = element_blank())
bot <- ggplot() + vregs +
  geom_hline(yintercept = 1.3, color = "grey30", linewidth = .35, linetype = "dotted") +
  geom_hline(yintercept = 1, color = "grey55", linewidth = .3) +
  annotate("text", x = as.Date("2005-01-01"), y = 1.3, label = "1,3 · valor de mercado", hjust = 0, vjust = -0.4, size = 3, color = "grey30") +
  geom_line(data = rt, aes(mes, y, linetype = serie), color = "grey15", linewidth = .6) +
  scale_linetype_manual(values = c("TC oficial" = "solid", "dólar paralelo" = "22")) + xdate +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(x = NULL, y = "Súper / Brent (veces)", linetype = "Cociente con") + theme_k +
  theme(legend.position = "bottom")
gK1b <- stack2(top, bot)
save_stack(gK1b, fs::path(DIR_TMP, "figK1b_super_vs_brent_pesos.png"), height = 8)
ok <- file.copy(fs::path(DIR_TMP, "figK1b_super_vs_brent_pesos.png"), fs::path(DIR_FIG, "figK1b_super_vs_brent_pesos.png"), overwrite = TRUE)
cat("saved: figK1b_super_vs_brent_pesos.png | copied to output folder:", ok, "\n")
cat("\nK1b: annual change (%) of the pump price and of Brent, both in pesos, by year\n")
yr <- d[, .(super = last(p_sin_ars), brent_of = last(brent_ars_of), brent_bl = last(brent_ars_bl)), by = .(anio = year(mes))]
yr[, `:=`(d_super = round(100 * (super / shift(super) - 1)), d_brent_of = round(100 * (brent_of / shift(brent_of) - 1)), d_brent_bl = round(100 * (brent_bl / shift(brent_bl) - 1)))]
print(yr[!is.na(d_super), .(anio, d_super, d_brent_of, d_brent_bl)])

# 5.6 Monthly series (CSV, kept for reuse) and diagnostics ----
out <- d[, .(mes, regime, bocas, tc_a3500=tc, tc_blue, p_super_sin_ars_l=p_sin_ars, p_super_con_ars_l=p_con_ars,
             super_sin_usd_l_blue=super_usd_bl, super_con_usd_l_blue=super_con_usd_bl,
             super_sin_usd_l_oficial=super_usd_of, super_con_usd_l_oficial=super_con_usd_of, brent_usd_bbl, brent_usd_l,
             fob_usgc_usd_gal=fob_usd_gal, fob_usgc_usd_l=fob_usd_l,
             paridad_obs_usd_m3=paridad_usd_m3, m3_impo, paridad_usd_l, paridad_fuente, ipc_2004_100)]
fwrite(out, fs::path(DIR_COV, "serie_super_vs_costos.csv"))
cat("CSV:", fs::path(DIR_COV, "serie_super_vs_costos.csv"), "\n")

cat("\nCorrelation (log levels) of the pump price with each reference\n")
print(d[, .(brent=round(cor(log(super_usd_l), log(brent_usd_l)),3),
            fob=round(cor(log(super_usd_l), log(fob_usd_l)),3),
            paridad=round(cor(log(super_usd_l), log(paridad_usd_l)),3))])
cat("\nBy regime: pump price in USD/l (parallel and official rate), references and gaps (medians)\n")
print(d[, .(meses=.N, super_blue=round(median(super_usd_bl),3), super_of=round(median(super_usd_of),3),
            fob=round(median(fob_usd_l),3), paridad=round(median(paridad_usd_l),3),
            ratio_brent=round(median(super_usd_bl/brent_usd_l),2),
            brecha_fob=percent(median(super_usd_bl/fob_usd_l-1), accuracy=1),
            brecha_par_blue=percent(median(super_usd_bl/paridad_usd_l-1), accuracy=1),
            brecha_par_of=percent(median(super_usd_of/paridad_usd_l-1), accuracy=1)), by=regime])
cat("\nBy year: pump price (parallel and official rate), gap over FOB and over parity (median, parallel rate)\n")
print(d[, .(super_blue=round(median(super_usd_bl),3), super_of=round(median(super_usd_of),3),
            fob=round(median(fob_usd_l),3), paridad=round(median(paridad_usd_l),3),
            obs=sum(paridad_fuente=="observada"),
            brecha_fob=percent(median(super_usd_bl/fob_usd_l-1), accuracy=1),
            brecha_par=percent(median(super_usd_bl/paridad_usd_l-1), accuracy=1),
            brecha_par_of=percent(median(super_usd_of/paridad_usd_l-1), accuracy=1)), by=.(anio=year(mes))])
cat("\nFigures K1-K4 and monthly series done.\n")


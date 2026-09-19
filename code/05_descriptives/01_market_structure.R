# 01_market_structure.R
# Descriptive tables and figures on market structure from the final analysis
# panel: outlets, operators, brands, business types, volumes and prices.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
# Output: 28 LaTeX tables in <DIR_OUTPUT>/Tablas and 15 PNG figures in
#         <DIR_OUTPUT>/Gráficos

library(data.table)
library(fs)
library(knitr)
library(ggplot2)

source("code/00_config.R")

options(scipen = 999)

# Paths ----

DIR_OUT_NEW <- DIR_OUTPUT
DIR_TABLES  <- fs::path(DIR_OUT_NEW, "Tablas")
DIR_FIGURES <- fs::path(DIR_OUT_NEW, "Gráficos")
DIR_INPUT   <- DIR_INTERIM

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

# 03_quantities_shares.R
# Quantities, market shares and reporting gaps in the station panel: reporting
# gaps (Q1), shares in volume vs. outlets (Q2), volume HHI (Q3), outlet size (Q4).
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: tables 29-32 (Tablas/) and figQ1-figQ4 (Gráficos/)

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales); library(fs); library(knitr)
})
source("code/00_config.R")

# Paths (same layout as 01_market_structure.R) ----
DIR_OUT_NEW <- DIR_OUTPUT
DIR_TABLES  <- fs::path(DIR_OUT_NEW, "Tablas")
DIR_FIGURES <- fs::path(DIR_OUT_NEW, "Gráficos")
DIR_INPUT   <- DIR_INTERIM
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

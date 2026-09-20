# figure_readme.R
# The figure on the front page of the repository, in English. It is the same
# estimate as figure R3 of 10_descriptives.R, grouped by government rather than
# by pricing regime: the mean price gap against the large private brands, taken
# within a locality and month, with the unbranded outlets as a second comparison.
#
# The figures of the thesis stay in Spanish; this one is drawn apart so that the
# front page can be read by someone who does not speak it.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
# Output: docs/figures/ypf_private_gap_by_government.png
#         docs/figures/pump_price_vs_brent.png

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

source("code/00_config.R")

OUT <- file.path("docs", "figures", "ypf_private_gap_by_government.png")

# Sample ----
# Same brand groups, products and price as section 2 of 10_descriptives.R.
PRIV   <- c("SHELL C.A.P.S.A.", "ESSO PETROLERA ARGENTINA S.R.L", "AXION", "PETROBRAS",
            "Pampa Energia", "PUMA", "OIL COMBUSTIBLES S.A.")
BLANCA <- c("BLANCA", "SIN EMPRESA BANDERA")
PRODS  <- c("Nafta (súper) entre 92 y 95 Ron", "Gas Oil Grado 2")

b <- readRDS(file.path(DIR_INTERIM, "eess_all_cleaned7_alternative_sinceappearance.rds"))
setDT(b)
b <- b[canal_de_comercializacion == "Al público"]
b[, precio := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))]
b <- b[!is.na(precio) & precio > 0 & producto %in% PRODS]
b[, grupo := fifelse(bandera == "YPF", "YPF",
              fifelse(bandera %in% PRIV, "Priv",
               fifelse(bandera %in% BLANCA, "Blanca", "Otras")))]

# Governments ----
# The blocks of the analysis plan. The 2019 freeze is kept apart from the rest
# of the Macri years because prices were set by decree over those four months.
gov_of <- function(d) factor(
  fifelse(d <  as.Date("2012-05-01"), "Repsol-owned\n2005-2012",
   fifelse(d <  as.Date("2016-01-01"), "State-owned\nFernández de Kirchner\n2012-2015",
    fifelse(d <  as.Date("2019-08-01"), "Macri\n2016-2019",
     fifelse(d <  as.Date("2019-12-01"), "Price freeze\nAug-Nov 2019",
      fifelse(d <  as.Date("2023-12-01"), "Fernández\n2020-2023", "Milei\n2024"))))),
  levels = c("Repsol-owned\n2005-2012", "State-owned\nFernández de Kirchner\n2012-2015",
             "Macri\n2016-2019", "Price freeze\nAug-Nov 2019",
             "Fernández\n2020-2023", "Milei\n2024"))

# Mean price of each group by locality, month and product ----
cm <- b[, .(p_ypf  = mean(precio[grupo == "YPF"]),
            p_priv = mean(precio[grupo == "Priv"]),
            p_blan = mean(precio[grupo == "Blanca"])),
        by = .(producto, provincia, localidad, periodo_dt)]
cm[, gov := gov_of(periodo_dt)]

cm[, g_priv := p_ypf  / p_priv - 1]
cm[, g_blan := p_blan / p_priv - 1]
for (v in c("g_priv", "g_blan")) cm[abs(get(v)) > 0.4, (v) := NA_real_]

# Estimate and 95% interval, clustered by locality ----
# The gap is averaged within a locality first, so each locality counts once.
# Localities are keyed with their province: two provinces can use the same name.
build <- function(col, lab) {
  loc <- cm[!is.na(get(col)), .(g = mean(get(col))), by = .(producto, gov, provincia, localidad)]
  loc[, .(est = mean(g), se = sd(g)/sqrt(.N), nloc = .N), by = .(producto, gov)][, grp := lab]
}
r <- rbind(build("g_priv", "YPF"), build("g_blan", "Unbranded outlets"))
r[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se)]
r[, producto_en := factor(fifelse(producto == "Gas Oil Grado 2", "Diesel (grade 2)", "Regular gasoline"),
                          levels = c("Regular gasoline", "Diesel (grade 2)"))]
r[, grp := factor(grp, levels = c("YPF", "Unbranded outlets"))]

print(r[, .(producto_en, gov, grp, est = round(100*est, 2), nloc)][order(producto_en, grp, gov)])

# Figure ----
p <- ggplot(r, aes(gov, est, color = grp)) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = .4) +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = .7,
                 position = position_dodge(width = .55)) +
  geom_point(size = 2.6, position = position_dodge(width = .55)) +
  facet_wrap(~producto_en, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("YPF" = "#1F3864", "Unbranded outlets" = "#2E7D32")) +
  labs(
    title = "YPF's price gap tracks the government, not who owns the firm",
    subtitle = paste0("Mean gap against the large private brands, within the same locality and month; ",
                      "negative means cheaper.\n95% intervals, localities as clusters. ",
                      "Pre-tax price at the pump."),
    x = NULL, y = "Gap against the large private brands", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        legend.justification = "left",
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey30", size = 9.5, lineheight = 1.15),
        plot.title.position = "plot",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(size = 8.5, lineheight = 0.95))

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT, p, width = 9.5, height = 6.4, dpi = 200)
cat("saved:", OUT, "\n")

# Second figure: the pump price against crude ----
# The national median pre-tax price of regular gasoline against Brent, both in
# dollars per litre. It repeats figure K1 of 10_descriptives.R with English
# labels; the series it reads is the one that section 5 writes.

SERIE <- file.path(DIR_COVAR, "serie_super_vs_costos.csv")

if (!file.exists(SERIE)) {
  cat("serie_super_vs_costos.csv not found; run section 5 of 10_descriptives.R first\n")
} else {
  s <- fread(SERIE, encoding = "UTF-8")
  s[, mes := as.Date(mes)]

  # The parallel rate is the one that makes the series comparable over time: the
  # official rate was held far from the market rate for much of the period.
  long <- rbind(
    s[!is.na(super_sin_usd_l_blue), .(mes, usd_l = super_sin_usd_l_blue, serie = "Regular gasoline, pre-tax")],
    s[!is.na(brent_usd_l),          .(mes, usd_l = brent_usd_l,          serie = "Brent crude")])
  long[, serie := factor(serie, levels = c("Regular gasoline, pre-tax", "Brent crude"))]

  ratio <- s[!is.na(super_sin_usd_l_blue) & !is.na(brent_usd_l),
             .(mes, r = super_sin_usd_l_blue / brent_usd_l)]

  BREAKS <- data.frame(x = as.Date(c("2012-05-01", "2017-10-01", "2019-08-01")))

  top <- ggplot(long, aes(mes, usd_l, color = serie)) +
    geom_vline(data = BREAKS, aes(xintercept = x), linetype = "dashed", color = "grey60", linewidth = .3) +
    geom_line(linewidth = .6) +
    scale_color_manual(values = c("Regular gasoline, pre-tax" = "#1F3864", "Brent crude" = "#C55A11")) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(title = "The pump price of gasoline and the price of crude, in dollars",
         subtitle = paste0("National median pre-tax price of regular gasoline against Brent, ",
                           "USD per litre at the parallel exchange rate.\nDashed lines: the 2012 ",
                           "renationalization, the 2017 deregulation and the 2019 price freeze."),
         x = NULL, y = "USD per litre", color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", legend.justification = "left",
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey30", size = 9.5, lineheight = 1.15),
          plot.title.position = "plot",
          panel.grid.minor = element_blank())

  bottom <- ggplot(ratio, aes(mes, r)) +
    geom_vline(data = BREAKS, aes(xintercept = x), linetype = "dashed", color = "grey60", linewidth = .3) +
    geom_hline(yintercept = 1, color = "grey45", linewidth = .3) +
    geom_line(linewidth = .55, color = "grey20") +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(x = NULL, y = "Gasoline / Brent") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # Stacked with gtable, which ggplot2 already depends on, after equalizing the
  # widths of the two panels. Same helper as section 5 of 10_descriptives.R.
  g1 <- ggplotGrob(top); g2 <- ggplotGrob(bottom)
  w <- grid::unit.pmax(g1$widths, g2$widths); g1$widths <- w; g2$widths <- w
  a1 <- g1$layout[g1$layout$name == "panel", "t"]; g1$heights[a1] <- grid::unit(2.1, "null")
  a2 <- g2$layout[g2$layout$name == "panel", "t"]; g2$heights[a2] <- grid::unit(1, "null")
  g <- rbind(g1, g2, size = "first")

  png(file.path("docs", "figures", "pump_price_vs_brent.png"),
      width = 9.5, height = 6.4, units = "in", res = 200)
  grid::grid.draw(g)
  dev.off()
  cat("saved: docs/figures/pump_price_vs_brent.png\n")
}

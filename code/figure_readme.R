# figure_readme.R
# The two figures on the front page of the repository, in English. The figures of
# the thesis stay in Spanish; these are drawn apart so that the front page can be
# read by someone who does not read it.
#
# Figure 1 repeats the estimate of figure R3 of 10_descriptives.R, grouped by
# government rather than by pricing regime, with the monthly series left visible
# behind it. Figure 2 repeats figure K1 of section 5.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
#         serie_super_vs_costos.csv (written by section 5 of 10_descriptives.R)
# Output: docs/figures/ypf_private_gap_by_government.png
#         docs/figures/pump_price_vs_brent.png

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

source("code/00_config.R")

DIR_FIG <- file.path("docs", "figures")
dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)

# Figure 1: the gap against the large private brands ----

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

# Mean price of each group by locality, month and product, then the gap. Gaps
# beyond 40% in absolute value are treated as gross errors, as in section 2.
cm <- b[, .(p_ypf  = mean(precio[grupo == "YPF"]),
            p_priv = mean(precio[grupo == "Priv"]),
            p_blan = mean(precio[grupo == "Blanca"])),
        by = .(producto, provincia, localidad, periodo_dt)]
cm[, periodo_dt := as.Date(periodo_dt)]
cm[, g_priv := p_ypf  / p_priv - 1]
cm[, g_blan := p_blan / p_priv - 1]
for (v in c("g_priv", "g_blan")) cm[abs(get(v)) > 0.4, (v) := NA_real_]

# The blocks of the analysis plan. The 2019 freeze is kept apart from the rest of
# the Macri years because prices were set by decree over those four months.
CUTS <- as.Date(c("2004-12-01", "2012-05-01", "2016-01-01", "2019-08-01",
                  "2019-12-01", "2023-12-01", "2025-01-01"))
LABS <- c("Repsol-owned", "State-owned\nFernández de Kirchner", "Macri",
          "Freeze", "Fernández", "Milei")
cm[, gov := cut(periodo_dt, breaks = CUTS, labels = LABS, right = FALSE)]

# Median across localities, month by month
ts <- cm[!is.na(g_priv), .(gap = median(g_priv)), by = .(producto, periodo_dt)]

# Mean of each government. The gap is averaged within a locality first, so every
# locality counts once whatever its size; the interval is the spread of those
# locality means. Localities are keyed with their province, since two provinces
# can use the same name.
loc <- cm[!is.na(g_priv), .(gl = mean(g_priv)), by = .(producto, gov, provincia, localidad)]
seg <- loc[, .(est = mean(gl), se = sd(gl)/sqrt(.N), nloc = .N), by = .(producto, gov)]
seg[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se, gi = as.integer(gov))]
seg[, `:=`(x0 = CUTS[gi], x1 = CUTS[gi + 1])]

# The unbranded outlets are reported in the caption rather than drawn, to keep
# the figure readable.
blan <- cm[!is.na(g_blan), .(gl = mean(g_blan)), by = .(producto, gov, provincia, localidad)
           ][, .(est = round(100*mean(gl), 2)), by = .(producto, gov)]
cat("Unbranded outlets against the large private brands, mean by government (%):\n")
print(blan)

en <- function(x) factor(fifelse(x == "Gas Oil Grado 2", "Diesel (grade 2)", "Regular gasoline"),
                         levels = c("Regular gasoline", "Diesel (grade 2)"))
ts[, prod_en := en(producto)]
seg[, prod_en := en(producto)]

shade <- data.table(x0 = CUTS[-length(CUTS)], x1 = CUTS[-1])[c(2, 4, 6)]
tags  <- data.table(x = CUTS[-length(CUTS)] + diff(CUTS)/2, y = 0.058, lab = LABS,
                    prod_en = factor("Regular gasoline",
                                     levels = c("Regular gasoline", "Diesel (grade 2)")))

g1 <- ggplot() +
  geom_rect(data = shade, aes(xmin = x0, xmax = x1, ymin = -Inf, ymax = Inf),
            fill = "grey92", alpha = .6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = .35) +
  geom_line(data = ts, aes(periodo_dt, gap), color = "grey58", linewidth = .35) +
  geom_rect(data = seg, aes(xmin = x0, xmax = x1, ymin = lo, ymax = hi),
            fill = "#1F3864", alpha = .25) +
  geom_segment(data = seg, aes(x = x0, xend = x1, y = est, yend = est),
               color = "#1F3864", linewidth = 1) +
  geom_text(data = tags, aes(x, y, label = lab), size = 2.5, color = "grey30",
            vjust = 1, lineheight = .9) +
  facet_wrap(~prod_en, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y",
               limits = c(CUTS[1], CUTS[length(CUTS)]), expand = c(0.005, 0)) +
  coord_cartesian(ylim = c(-0.145, 0.065)) +
  labs(title = "The gap is there under both owners, and closes in the years prices were free",
       subtitle = paste0("Price of YPF against the large private brands, within the same locality and month; ",
                         "negative means YPF is cheaper.\nGrey: median across localities, by month. ",
                         "Blue: mean of each government, with an interval from the spread of\nlocality means. ",
                         "Pre-tax price. Gaps beyond 40% are dropped as gross errors."),
       x = NULL, y = "Gap against the large private brands") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey30", size = 9, lineheight = 1.2),
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

ggsave(file.path(DIR_FIG, "ypf_private_gap_by_government.png"), g1,
       width = 9.6, height = 6.6, dpi = 200)
cat("saved:", file.path(DIR_FIG, "ypf_private_gap_by_government.png"), "\n")

# Figure 2: the pump price against crude ----

SERIE <- file.path(DIR_COVAR, "serie_super_vs_costos.csv")

if (!file.exists(SERIE)) {
  cat("serie_super_vs_costos.csv not found; run section 5 of 10_descriptives.R first\n")
} else {
  s <- fread(SERIE, encoding = "UTF-8")
  s[, mes := as.Date(mes)]

  # Brent is quoted in dollars. The pump price is in pesos and is converted at the
  # parallel rate, which is the one that keeps the series comparable over a period
  # when the official rate was held far from the market.
  long <- rbind(
    s[!is.na(super_sin_usd_l_blue), .(mes, usd_l = super_sin_usd_l_blue,
                                      serie = "Regular gasoline, pre-tax")],
    s[!is.na(brent_usd_l), .(mes, usd_l = brent_usd_l, serie = "Brent crude")])
  long[, serie := factor(serie, levels = c("Regular gasoline, pre-tax", "Brent crude"))]

  ratio <- s[!is.na(super_sin_usd_l_blue) & !is.na(brent_usd_l),
             .(mes, r = super_sin_usd_l_blue / brent_usd_l)]

  BREAKS <- data.frame(x = as.Date(c("2012-05-01", "2017-10-01", "2019-08-01")))

  top <- ggplot(long, aes(mes, usd_l, color = serie)) +
    geom_vline(data = BREAKS, aes(xintercept = x), linetype = "dashed",
               color = "grey60", linewidth = .3) +
    geom_line(linewidth = .6) +
    scale_color_manual(values = c("Regular gasoline, pre-tax" = "#1F3864",
                                  "Brent crude" = "#C55A11")) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(title = "The pump price of gasoline and the price of crude, in dollars",
         subtitle = paste0("National median pre-tax price of regular gasoline, converted at the ",
                           "parallel exchange rate, against Brent.\nDashed lines: the 2012 ",
                           "renationalization, the 2017 deregulation and the 2019 price freeze."),
         x = NULL, y = "USD per litre", color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", legend.justification = "left",
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey30", size = 9, lineheight = 1.2),
          plot.title.position = "plot",
          panel.grid.minor = element_blank())

  bottom <- ggplot(ratio, aes(mes, r)) +
    geom_vline(data = BREAKS, aes(xintercept = x), linetype = "dashed",
               color = "grey60", linewidth = .3) +
    geom_hline(yintercept = 1, color = "grey45", linewidth = .3) +
    geom_line(linewidth = .55, color = "grey20") +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(x = NULL, y = "Gasoline / Brent") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # Stacked with gtable, which ggplot2 already depends on, after equalizing the
  # widths of the two panels. Same approach as section 5 of 10_descriptives.R.
  q1 <- ggplotGrob(top); q2 <- ggplotGrob(bottom)
  w <- grid::unit.pmax(q1$widths, q2$widths); q1$widths <- w; q2$widths <- w
  a1 <- q1$layout[q1$layout$name == "panel", "t"]; q1$heights[a1] <- grid::unit(2.1, "null")
  a2 <- q2$layout[q2$layout$name == "panel", "t"]; q2$heights[a2] <- grid::unit(1, "null")
  q <- rbind(q1, q2, size = "first")

  png(file.path(DIR_FIG, "pump_price_vs_brent.png"),
      width = 9.6, height = 6.6, units = "in", res = 200)
  grid::grid.draw(q)
  dev.off()
  cat("saved:", file.path(DIR_FIG, "pump_price_vs_brent.png"), "\n")
}

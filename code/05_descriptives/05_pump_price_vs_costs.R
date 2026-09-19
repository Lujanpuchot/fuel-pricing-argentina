# 05_pump_price_vs_costs.R
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

suppressPackageStartupMessages({library(data.table); library(ggplot2); library(scales); library(fs)})
source("code/00_config.R")
pdf(NULL)  # keeps ggplotGrob() from leaving an Rplots.pdf in the working directory

# Figures are saved in a local temporary folder and copied to the synced output
# folder right away, because the sync client locks files while they are written.
DIR_INPUT <- DIR_INTERIM
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

# 1. Monthly price of regular gasoline (national median, retail channel) ----
b <- readRDS(FILE_BASE); setDT(b)
s <- b[canal_de_comercializacion=="Al público" & producto==PROD_SUPER]
rm(b); invisible(gc())
s[, p_sin := suppressWarnings(as.numeric(as.character(precio_sin_impuestos)))]
s[, p_con := suppressWarnings(as.numeric(as.character(precio_con_impuestos)))]
s <- s[!is.na(p_sin) & p_sin>0]
ps <- s[, .(p_sin_ars=median(p_sin), p_con_ars=median(p_con[!is.na(p_con) & p_con>0]), bocas=uniqueN(nro_inscripcion)),
        by=.(mes=as.Date(periodo_dt))][order(mes)]
cat("Regular gasoline price:", nrow(ps), "months,", format(min(ps$mes)), "-", format(max(ps$mes)), "\n")

# 2. National series and FOB USGC ----
sn <- fread(fs::path(DIR_COV, "covar_series_nacionales.csv"))
sn[, mes := as.Date(mes)]
fob <- fread(fs::path(DIR_COV, "raw_series/usgc_gasolina_fob_fred.csv"))
setnames(fob, c("mes","fob_usd_gal")); fob[, mes := as.Date(mes)]
# Parallel exchange rate (blue, selling rate, monthly average; Ámbito, 2004-2024).
# Before 2011 it coincides with the official rate.
blue <- fread(fs::path(DIR_COV, "raw_series/dolar_blue_mensual_ambito.csv"))[, .(mes=as.Date(mes), tc_blue=blue_venta_prom)]

# 3. Observed import parity (SESCO imports of súper and ultra gasoline) ----
ce <- fread(fs::path(DIR_COV, "covar_sesco_comercio_ext.csv"))
par_obs <- ce[tipo=="Importación" & producto %in% c("Nafta Grado 2 (Súper)(m3)","Nafta Grado 3 (Ultra)(m3)") &
              cantidad>0 & monto_usd>0,
              .(paridad_usd_m3 = sum(monto_usd)/sum(cantidad), m3_impo=sum(cantidad)),
              by=.(mes=as.Date(sprintf("%d-%02d-01", anio, mes)))]
# Unit values outside [200, 2000] USD per m3 are data-entry errors and are dropped
par_obs <- par_obs[paridad_usd_m3 >= 200 & paridad_usd_m3 <= 2000]
cat("Observed parity:", nrow(par_obs), "months between", format(min(par_obs$mes)), "and", format(max(par_obs$mes)), "\n")

# 4. Monthly panel ----
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

# 5. Figures K1-K3 ----
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

# 6. Monthly series (CSV, kept for reuse) and diagnostics ----
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

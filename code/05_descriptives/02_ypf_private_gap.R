# 02_ypf_private_gap.R
# Robustness checks (figures R1-R4) of figure 1, the price gap between YPF (the
# state-controlled firm) and rival brands.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
#         (boca (outlet) x product x channel x month)
# Output: figR1_benchmark_shell.png, figR2_ladder_blancas.png,
#         figR3_gap_condicional_IC.png, figR4_distribucion_gap.png (Gráficos/)

suppressPackageStartupMessages({library(data.table); library(ggplot2); library(scales)})
source("code/00_config.R")

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

# 04_station_characteristics.R
# Inventory of observable station characteristics, the X of the BLP demand model:
# variation within and between markets (C1, C2) and highway vs. urban location (C3).
#
# Input:  eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
# Output: tables 33-34 (Tablas/), figC1 and figC2 (Gráficos/)

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

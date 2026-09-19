# unit_cost.R
# Upstream unit cost of fuel (cU) for nafta (gasoline) and gasoil (diesel),
# national, monthly and quarterly, plus a provincial step (delta_prov) estimated
# from wholesale prices.
#
# Input:  raw_costo/Informe_Regalias_CRUDO.xlsx (downloaded if missing),
#         covar_sesco_refinacion.csv, covar_series_nacionales.csv,
#         raw_costo/20f_ypf_extract.csv and the biofuel price files in raw_costo/,
#         precios_mayoristas_res1104.csv and its December 2024 update
# Output: covar_costo_cu_mensual.csv, covar_costo_cu.csv,
#         covar_escalon_provincial.csv, raw_costo/chequeo_c3_margen_privadas.csv
#
# The cost is built in layers:
#     cU_g,t = P_crude_dom,t * (FOB_g,t / Brent_t) * (1 / yield)
#              + refining opex + regulated biofuel blend
#   - P_crude_dom: realized price of crude sold in the domestic market (royalties
#     report of the Energy Secretariat, USD/m3, monthly by crude type from
#     2006-01), weighted by the crude slate of the YPF refineries (SESCO).
#     Realized prices differ from the prices set by decree, so the decreed
#     anchors are only kept as reference columns.
#   - FOB_g / Brent: netback split of the barrel with the USGC FOB prices of
#     covar_series_nacionales.csv (diesel already spliced before June 2006).
#   - yield: by-products over inputs of the YPF refineries (SESCO), annual.
#   - Refining opex: YPF 20-F filings; a provisional 5 USD/bbl if the extract is
#     missing (flagged in fuente_opex).
#   - Biofuels: mandatory blend share times the regulated price. When the price
#     of a month is missing, the blended fraction is valued at the fossil cost
#     (flagged in fuente_bio), which is conservative.
#
# Units are USD/m3 unless stated otherwise; *_ars_l is pesos per liter at the
# A 3500 exchange rate. The model window is 2004-12 to 2024-12.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
  library(readxl)
})
source("code/00_config.R")

DIR  <- DIR_COVAR
RAWC <- fs::path(DIR, "raw_costo")
MAYO <- fs::path(DIR_WHOLESALE, "precios_mayoristas_res1104.csv")
if (!dir.exists(RAWC)) dir.create(RAWC)
GAL_M3 <- 264.172        # gallons per m3
BBL_M3 <- 6.28981        # barrels per m3
DENS_BIODIESEL <- 0.88   # kg/l (FAME), so 1 t = 1136.4 l

# 1. Realized crude price: royalties report (Energy Secretariat) ----
# CKAN dataset regalias-de-petroleo-crudo-... The resource "Precios de Escalante"
# points to the same zip, and its sheet "Tabla precios" carries every crude type.
xls <- fs::path(RAWC, "Informe_Regalias_CRUDO.xlsx")
if (!file.exists(xls)) {
  zipf <- fs::path(RAWC, "Regalias_CRUDO.zip")
  download.file("http://www.energia.gob.ar/contenidos/archivos/Reorganizacion/informacion_del_mercado/mercado_hidrocarburos/informacion_estadistica/regalias/Regalias_CRUDO.zip",
                zipf, mode = "wb", quiet = TRUE)
  unzip(zipf, exdir = fs::path(RAWC, "tmp_regalias"))
  file.copy(list.files(fs::path(RAWC, "tmp_regalias"), pattern = "\\.xlsx$", full.names = TRUE)[1], xls)
  unlink(fs::path(RAWC, "tmp_regalias"), recursive = TRUE)
}
d <- suppressMessages(readxl::read_excel(xls, sheet = "Tabla precios", col_names = FALSE))
setDT(d)
hdr <- as.character(unlist(d[13, ]))          # header row: AÑO | MES | crude types...
stopifnot(hdr[2] == "MES")
nm <- c("anio", "mes", hdr[3:14])
dd <- d[14:.N]
setnames(dd, nm)
dd[, anio := nafill(as.integer(anio), type = "locf")]
dd <- dd[mes %in% as.character(1:12)][, mes := as.integer(mes)]
for (c in nm[3:14]) set(dd, j = c, value = as.numeric(dd[[c]]))
PR <- melt(dd, id.vars = c("anio", "mes"), variable.name = "tipo_crudo", value.name = "precio_usd_m3")
PR[precio_usd_m3 == 0, precio_usd_m3 := NA]   # zeros mean no data (before 2006, future months)
cat("[royalties] prices by crude type:", PR[!is.na(precio_usd_m3) & !tipo_crudo %in% c("BRENT","WTI"), uniqueN(tipo_crudo)],
    "types | coverage", PR[!is.na(precio_usd_m3) & !tipo_crudo %in% c("BRENT","WTI"), paste(range(anio*100+mes), collapse = " to ")], "\n")

# Check against two known regulated prices: the support price for Medanito in
# 2015 (about 77 USD/bbl) and the "barril criollo", the domestic reference price
# of Decree 488/2020 (45 USD/bbl)
v15 <- PR[anio == 2015 & tipo_crudo == "MEDANITO", mean(precio_usd_m3, na.rm = TRUE)]/BBL_M3
v20 <- PR[anio == 2020 & mes %in% 6:12 & tipo_crudo == "TOTAL TIPO DE CRUDO", mean(precio_usd_m3, na.rm = TRUE)]/BBL_M3
cat(sprintf("  2015 Medanito: %.1f USD/bbl (support price about 77) | 2020 Jun-Dec TOTAL: %.1f (barril criollo 45, realized price lower)\n", v15, v20))

# 2. Crude slate of the YPF refineries (SESCO refining) -> national crude price by month ----
ref <- fread(fs::path(DIR, "covar_sesco_refinacion.csv"), encoding = "UTF-8")
map <- c("Cuenca Neuquina - Neuquen (Medanito)"="MEDANITO", "Cuenca Neuquina - Rio Negro (Medanito)"="MEDANITO",
         "Cuenca Neuquina - La Pampa (Medanito)"="MEDANITO", "Cuenca Neuquina - Mendoza"="MEDANITO",
         "Cuenca Golfo San Jorge - Cañadón Seco"="CAÑADON SECO", "Cuenca Golfo San Jorge - Chubut (Escalante)"="ESCALANTE",
         "Cuenca Cuyana y Bolsones"="MENDOZA NORTE", "Cuenca Noroeste - Salta"="NOROESTE",
         "Cuenca Noroeste - Jujuy"="NOROESTE", "Cuenca Noroeste - Formosa"="NOROESTE",
         "Cuenca Austral - Tierra l Fuego - San Sebastián"="SAN SEBASTIÁN",
         "Cuenca Austral - Tierra l Fuego - Off Shore (Hidra)"="HIDRA",
         "Cuenca Austral - Santa Cruz - On  Shore"="MARÍA INÉS", "Cuenca Austral - Santa Cruz - Off Shore"="MAGALLANES",
         "Crudo importado"="IMPORTADO")
cue <- ref[bloque == "carga" & concepto %in% names(map) & grepl("YPF", empresa)]
cue[, tipo_crudo := map[concepto]]
# The Austral basin types are only priced from 2019 on; before that the report groups them under HIDRA
cue[anio < 2019 & tipo_crudo %in% c("SAN SEBASTIÁN","MARÍA INÉS","MAGALLANES"), tipo_crudo := "HIDRA"]
dieta <- cue[, .(m3 = sum(cantidad_m3, na.rm = TRUE)), by = .(anio, mes, tipo_crudo)]
# Imported crude is valued at Brent (YPF is a net buyer; imports are below 2% of the slate)
PRw <- rbind(PR[!tipo_crudo %in% c("BRENT","WTI","TOTAL TIPO DE CRUDO")],
             PR[tipo_crudo == "BRENT", .(anio, mes, tipo_crudo = "IMPORTADO", precio_usd_m3)])
pm <- merge(dieta, PRw, by = c("anio","mes","tipo_crudo"))[!is.na(precio_usd_m3)]
PY <- pm[, .(p_crudo_ypf = sum(m3*precio_usd_m3)/sum(m3)), by = .(anio, mes)]
# 2006-2009, before SESCO starts: fixed slate equal to the 2010-2011 average
wfix <- dieta[anio %in% 2010:2011, .(m3 = sum(m3)), by = tipo_crudo][, share := m3/sum(m3)]
pre <- merge(PRw[anio %in% 2006:2009], wfix[, .(tipo_crudo, share)], by = "tipo_crudo")
pre <- pre[!is.na(precio_usd_m3), .(p_crudo_ypf = sum(share*precio_usd_m3)/sum(share)), by = .(anio, mes)]
PY <- rbind(PY, pre)[order(anio, mes)]
PY <- merge(PY, PR[tipo_crudo == "TOTAL TIPO DE CRUDO", .(anio, mes, p_crudo_total_regalias = precio_usd_m3)],
            by = c("anio","mes"), all.x = TRUE)
PY[, fuente_crudo := fifelse(anio <= 2009, "regalías × dieta YPF fija 2010-11", "regalías × dieta YPF SESCO mensual")]
cc <- PY[anio >= 2010]
cat(sprintf("[crude price] YPF slate vs TOTAL of the royalties report: cor %.3f, mean ratio %.3f\n",
            cor(cc$p_crudo_ypf, cc$p_crudo_total_regalias), mean(cc$p_crudo_ypf/cc$p_crudo_total_regalias)))

# 3. National series (Brent, exchange rate, FOB) and YPF refining yield ----
sn <- fread(fs::path(DIR, "covar_series_nacionales.csv"))
sn[, `:=`(anio = year(mes), mes_n = month(mes))]
SN <- sn[, .(anio, mes = mes_n, brent_usd_bbl, tc_a3500_prom,
             fob_naf_m3 = nafta_usgulf_usd_m3, fob_gas_m3 = gasoil_usgulf_emp_usd_m3)]
SN[, brent_usd_m3 := brent_usd_bbl * BBL_M3]

yy <- ref[grepl("YPF", empresa), .(m3 = sum(cantidad_m3, na.rm = TRUE)), by = .(anio, bloque)]
rend_a <- dcast(yy, anio ~ bloque, value.var = "m3")[, rend := subproducto/carga][]
rend_fix <- rend_a[anio %in% 2010:2012, mean(rend)]

M <- merge(SN, PY, by = c("anio","mes"), all.x = TRUE)
M <- merge(M, rend_a[, .(anio, rend)], by = "anio", all.x = TRUE)
M[is.na(rend), rend := rend_fix]
M <- M[anio*100 + mes >= 200412 & anio <= 2024][order(anio, mes)]

# Backcast 2004-12 to 2005-12: annual anchor of the 20-F if available, otherwise the 2006 ratio to Brent
f20 <- fs::path(RAWC, "20f_ypf_extract.csv")
r06 <- M[anio == 2006, mean(p_crudo_ypf/brent_usd_m3)]
if (file.exists(f20)) {
  t20 <- fread(f20)
  a05 <- t20[anio_fiscal == 2005, suppressWarnings(as.numeric(crudo_realizado_usd_bbl))]
  a04 <- t20[anio_fiscal == 2004, suppressWarnings(as.numeric(crudo_realizado_usd_bbl))]
} else a05 <- a04 <- numeric(0)
M[is.na(p_crudo_ypf), fuente_crudo := "retropolado ratio P/Brent 2006"]
M[is.na(p_crudo_ypf), p_crudo_ypf := brent_usd_m3 * r06]
# Rescale the 2005 level (and 2004, if reported) to the realized price of the
# 20-F, keeping the monthly shape of Brent
if (length(a05) && !is.na(a05)) {
  esc <- a05 * BBL_M3 / M[anio == 2005, mean(p_crudo_ypf)]
  M[anio == 2005, `:=`(p_crudo_ypf = p_crudo_ypf * esc, fuente_crudo = "retropolado 20-F 2005 × forma Brent")]
  if (length(a04) && !is.na(a04)) {
    esc4 <- a04 * BBL_M3 / M[anio == 2004, mean(p_crudo_ypf)]
    M[anio == 2004, `:=`(p_crudo_ypf = p_crudo_ypf * esc4, fuente_crudo = "retropolado 20-F 2004 × forma Brent")]
  }
}

# 4. Refining opex (20-F): annual, constant within the year ----
# 2004-2013: the 20-F reports an explicit "refining cost per barrel" in pesos per
#   barrel (MD&A of the refining and marketing segment; segment costs minus crude
#   purchases, divided by barrels produced). It is converted to USD at the annual
#   average A 3500 exchange rate.
# 2014-2024: the 20-F no longer reports the unit level, so the last level (2013)
#   is chained with the changes in unit cost that each 20-F does report (in ARS
#   up to 2021; in USD from 2022, "refining and logistics" segment). There is no
#   unit change for 2015 and 2020: the change in total cost adjusted by
#   throughput is used. 2024 has no percentage either: it is derived from an
#   increase of 189 million USD over about 1,600 million and throughput up 2.4%.
#   Details in raw_costo/20f_ypf_notas.md.
M[, `:=`(opex_usd_m3 = 5 * BBL_M3, fuente_opex = "PROVISORIO 5 USD/bbl (falta 20-F)")]
if (file.exists(f20)) {
  tc_a <- M[, .(tc = mean(tc_a3500_prom)), by = anio]         # annual average exchange rate (model window)
  tc_a <- rbind(tc_a, SN[anio %in% 2004:2013 & !anio %in% tc_a$anio,
                         .(tc = mean(tc_a3500_prom)), by = anio])[order(anio)]
  t20 <- fread(f20)
  exp0 <- t20[!is.na(suppressWarnings(as.numeric(refino_cash_cost))),
              .(anio = anio_fiscal, ps_bbl = as.numeric(refino_cash_cost))]   # 2004-2013, pesos per barrel
  # Changes in the unit refining cost, from the MD&A of each 20-F:
  gr_ars <- c("2014" = 1.450,  # +45% per unit, in ARS
              "2015" = 1.178 * 46.2 / 47.5,  # +17.8% total cost, adjusted by throughput
              "2016" = 1.442, "2017" = 1.211, "2018" = 1.315, "2019" = 1.918,
              "2020" = 1.182 * 44.1 / 37.4,  # +18.2% total cost, adjusted by throughput (COVID)
              "2021" = 1.346)
  gr_usd <- c("2022" = 1.251, "2023" = 1.100,
              "2024" = 1 + (189 / 1600) * (46.8 / 47.9))      # derived, see the note above
  op <- merge(exp0, tc_a, by = "anio")[, .(anio, opex_bbl = ps_bbl / tc)]
  op[, fuente := "20-F explícito (Ps/bbl ÷ TC A3500)"]
  ult_ps <- exp0[anio == max(anio), ps_bbl]
  a <- exp0[, max(anio)]
  for (yy in names(gr_ars)) {
    ult_ps <- ult_ps * gr_ars[[yy]]
    a <- as.integer(yy)
    op <- rbind(op, data.table(anio = a, opex_bbl = ult_ps / tc_a[anio == a, tc],
                               fuente = "20-F encadenado (variación unitaria ARS ÷ TC)"))
  }
  ult_usd <- op[anio == 2021, opex_bbl]
  for (yy in names(gr_usd)) {
    ult_usd <- ult_usd * gr_usd[[yy]]
    op <- rbind(op, data.table(anio = as.integer(yy), opex_bbl = ult_usd,
                               fuente = "20-F encadenado (variación unitaria USD, refining+logistics)"))
  }
  cat("[opex 20-F] USD/bbl by year:\n")
  print(op[, .(anio, opex_bbl = round(opex_bbl, 2))])
  M <- merge(M, op, by = "anio", all.x = TRUE)
  setorder(M, anio, mes)
  M[!is.na(opex_bbl), `:=`(opex_usd_m3 = opex_bbl * BBL_M3, fuente_opex = fuente)]
  M[, `:=`(opex_bbl = NULL, fuente = NULL)]
}

# Check: crude price (YPF slate, annual mean) against the realized price in the 20-F
if (file.exists(f20)) {
  t20v <- fread(f20)[, .(anio = anio_fiscal, p20f = suppressWarnings(as.numeric(crudo_realizado_usd_bbl)))]
  vv <- merge(M[, .(p_mio = mean(p_crudo_ypf) / BBL_M3), by = anio], t20v, by = "anio")
  vv[, dif_pct := round(100 * (p_mio / p20f - 1), 1)]
  cat("\n[crude vs 20-F] % difference by year (royalties series with the YPF slate vs realized price in the 20-F):\n")
  print(vv[, .(anio, regalias = round(p_mio, 1), f20 = p20f, dif_pct)])
}

# 5. Biofuels: mandatory blend x regulated price ----
# Mandatory blend shares. Biodiesel: Law 26.093 regime from January 2010 (B5);
# Res. 554/2010, B7 from July 2010; Res. 1125/2013 and 390/2014, B9 from January
# 2014 and B10 from February 2014; Law 27.640, B5 from August 2021; Res.
# 438/2022, B7.5 from June 2022 (the temporary extra blend of Decree 330/2022,
# COTAB, in the second half of 2022 is not included). Ethanol: E5 from January
# 2010; Res. 44/2014, 8.5% from March 2014, 9% from October, 9.5% from November
# and 10% from December 2014; Decree 543/2016, 12% from April 2016, which Law
# 27.640 keeps.
corte <- function(anio, mes, grupo) {
  ym <- anio*100 + mes
  if (grupo == "nafta") fcase(ym < 201001, 0, ym < 201403, .05, ym < 201410, .085, ym < 201411, .09,
                              ym < 201412, .095, ym < 201604, .10, default = .12)
  else fcase(ym < 201001, 0, ym < 201007, .05, ym < 201401, .07, ym < 201402, .09,
             ym < 202108, .10, ym < 202206, .05, default = .075)
}
M[, `:=`(b_naf = mapply(corte, anio, mes, "nafta"), b_gas = mapply(corte, anio, mes, "gasoil"))]

# Biofuel prices: CKAN CSVs (no longer updated) completed with the prices set in later resolutions
et <- fread(fs::path(RAWC, "biocombustible-precios-de-bioetanol.csv"))
et <- et[, .(anio, mes, p_et_ars_l = rowMeans(cbind(cania, maiz), na.rm = TRUE))]
bd <- fread(fs::path(RAWC, "biocombustible-precios-de-biodiesel.csv"))
# Biodiesel prices are fortnightly: monthly mean of the "grande" (large producers) category
bd <- bd[, .(p_bd_ars_t = mean(grande, na.rm = TRUE)), by = .(anio, mes)]
fbio <- fs::path(RAWC, "bio_precios_completado.csv")
if (file.exists(fbio)) {
  bc <- fread(fbio, encoding = "UTF-8")
  bc <- bc[!is.na(precio_ars)]
  et2 <- bc[grepl("bioetanol", producto), .(p_et_ars_l = mean(precio_ars)), by = .(anio, mes)]
  bd2 <- bc[producto == "biodiesel", .(p_bd_ars_t = mean(precio_ars)), by = .(anio, mes)]
  et <- rbind(et, et2[!et, on = c("anio","mes")])
  bd <- rbind(bd, bd2[!bd, on = c("anio","mes")])
}
M <- merge(M, et, by = c("anio","mes"), all.x = TRUE)
M <- merge(M, bd, by = c("anio","mes"), all.x = TRUE)
# In months without a new resolution the previous price stays in force (freezes:
# March and May 2019, and January-September 2020 for biodiesel, held at 44,121
# pesos per ton until the resolution of October 2020), hence the LOCF
setorder(M, anio, mes)
M[, `:=`(et_locf = is.na(p_et_ars_l) & !is.na(nafill(p_et_ars_l, type = "locf")),
         bd_locf = is.na(p_bd_ars_t) & !is.na(nafill(p_bd_ars_t, type = "locf")))]
M[, `:=`(p_et_ars_l = nafill(p_et_ars_l, type = "locf"),
         p_bd_ars_t = nafill(p_bd_ars_t, type = "locf"))]
M[, `:=`(p_et_usd_m3 = p_et_ars_l * 1000 / tc_a3500_prom,
         p_bd_usd_m3 = p_bd_ars_t * DENS_BIODIESEL / tc_a3500_prom)]

# 6. Assemble cU ----
M[, `:=`(cu_crudo_naf = p_crudo_ypf * (fob_naf_m3/brent_usd_m3) / rend,
         cu_crudo_gas = p_crudo_ypf * (fob_gas_m3/brent_usd_m3) / rend)]
M[, `:=`(cu_fosil_naf = cu_crudo_naf + opex_usd_m3,
         cu_fosil_gas = cu_crudo_gas + opex_usd_m3)]
M[, `:=`(fuente_bio_naf = fcase(b_naf == 0, "sin corte",
                                !is.na(p_et_usd_m3) & !et_locf, "precio regulado SE",
                                !is.na(p_et_usd_m3) & et_locf, "precio regulado SE (congelado, LOCF)",
                                default = "SIN PRECIO BIO: fracción a costo fósil"),
         fuente_bio_gas = fcase(b_gas == 0, "sin corte",
                                !is.na(p_bd_usd_m3) & !bd_locf, "precio regulado SE",
                                !is.na(p_bd_usd_m3) & bd_locf, "precio regulado SE (congelado, LOCF)",
                                default = "SIN PRECIO BIO: fracción a costo fósil"))]
M[, `:=`(cu_naf_usd_m3 = (1-b_naf)*cu_fosil_naf + b_naf*fcoalesce(p_et_usd_m3, cu_fosil_naf),
         cu_gas_usd_m3 = (1-b_gas)*cu_fosil_gas + b_gas*fcoalesce(p_bd_usd_m3, cu_fosil_gas))]
M[, `:=`(cu_naf_ars_l = cu_naf_usd_m3/1000*tc_a3500_prom,
         cu_gas_ars_l = cu_gas_usd_m3/1000*tc_a3500_prom)]

# Decreed price anchors, for reference only: cU uses the realized price
M[, `:=`(ancla_norma = fcase(anio*100+mes >= 200711 & anio*100+mes <= 201412, "Res. 394/2007 valor de corte",
                             anio >= 2015 & anio <= 2016, "Acuerdo sostén 2015-16 (Medanito)",
                             anio == 2020 & mes >= 5 & mes <= 8, "Dto. 488/2020 barril criollo",
                             default = NA_character_),
         ancla_usd_bbl = fcase(anio*100+mes >= 200711 & anio*100+mes <= 201412, 42,
                               anio >= 2015 & anio <= 2016, 77,
                               anio == 2020 & mes >= 5 & mes <= 8, 45,
                               default = NA_real_))]

# Long format by grade group
L <- rbind(
  M[, .(anio, mes, grupo_grado = "nafta", cu_usd_m3 = cu_naf_usd_m3, cu_ars_l = cu_naf_ars_l,
        cu_crudo_usd_m3 = cu_crudo_naf, opex_usd_m3, corte_bio = b_naf, p_bio_usd_m3 = p_et_usd_m3,
        p_crudo_ypf_usd_m3 = p_crudo_ypf, p_crudo_total_regalias_usd_m3 = p_crudo_total_regalias,
        crack_fob_brent = fob_naf_m3/brent_usd_m3, rendimiento = rend, brent_usd_bbl, tc_a3500_prom,
        fuente_crudo, fuente_opex, fuente_bio = fuente_bio_naf, ancla_norma, ancla_usd_bbl)],
  M[, .(anio, mes, grupo_grado = "gasoil", cu_usd_m3 = cu_gas_usd_m3, cu_ars_l = cu_gas_ars_l,
        cu_crudo_usd_m3 = cu_crudo_gas, opex_usd_m3, corte_bio = b_gas, p_bio_usd_m3 = p_bd_usd_m3,
        p_crudo_ypf_usd_m3 = p_crudo_ypf, p_crudo_total_regalias_usd_m3 = p_crudo_total_regalias,
        crack_fob_brent = fob_gas_m3/brent_usd_m3, rendimiento = rend, brent_usd_bbl, tc_a3500_prom,
        fuente_crudo, fuente_opex, fuente_bio = fuente_bio_gas, ancla_norma, ancla_usd_bbl)]
)[order(grupo_grado, anio, mes)]
fwrite(L, fs::path(DIR, "covar_costo_cu_mensual.csv"), bom = TRUE)
cat("[covar_costo_cu_mensual.csv]", nrow(L), "rows\n")

Q <- L[, .(cu_usd_m3 = mean(cu_usd_m3), cu_ars_l = mean(cu_ars_l), cu_crudo_usd_m3 = mean(cu_crudo_usd_m3),
           opex_usd_m3 = mean(opex_usd_m3), corte_bio = mean(corte_bio),
           p_crudo_ypf_usd_m3 = mean(p_crudo_ypf_usd_m3), crack_fob_brent = mean(crack_fob_brent),
           brent_usd_bbl = mean(brent_usd_bbl), tc_a3500_prom = mean(tc_a3500_prom),
           ancla_norma = ancla_norma[!is.na(ancla_norma)][1], ancla_usd_bbl = mean(ancla_usd_bbl, na.rm = TRUE),
           meses_sin_precio_bio = sum(corte_bio > 0 & grepl("SIN PRECIO", fuente_bio))),
       by = .(grupo_grado, anio, trimestre = (mes-1) %/% 3 + 1)]
Q[is.nan(ancla_usd_bbl), ancla_usd_bbl := NA]
fwrite(Q, fs::path(DIR, "covar_costo_cu.csv"), bom = TRUE)
cat("[covar_costo_cu.csv]", nrow(Q), "rows (", Q[, uniqueN(paste(anio, trimestre))], "quarters x 2 grade groups )\n")

# 7. Provincial step delta_prov ----
# Log wholesale prices (w) of the private refiners in the resale channel,
# demeaned within operator x product x month and averaged by province and
# quarter, always weighting by volume. Prices outside the 1st-99th percentiles of
# each product-month are dropped. Prices reported under Res. 1104 include freight
# (delivered at destination), so delta picks up the differential in delivered cost.
d4 <- fread(MAYO, encoding = "UTF-8")
# December 2024 comes in the new-format file (fecha/periodo columns, taxes itemized by row)
f24 <- fs::path(DIR_WHOLESALE, "precios_mayoristas_res1104_desde_dic2024.csv")
if (file.exists(f24)) {
  n24 <- fread(f24, encoding = "UTF-8")
  # precio_sin_iimpuestos is how the source file spells the column
  n24 <- n24[periodo == "2024/12",
             .(anio = 2024L, mes = 12L, operador, provincia, producto, canal_de_comercializacion,
               precio_sin_impuestos = precio_sin_iimpuestos, volumen)]
  d4 <- rbind(d4[, .(anio, mes, operador, provincia, producto, canal_de_comercializacion,
                     precio_sin_impuestos, volumen)], n24)
  cat("[1104] added 2024-12 from the new-format file:", nrow(n24), "rows\n")
}
privadas <- c("SHELL COMPAÑIA ARGENTINA DE PETRÓLEO S.A.","ESSO PETROLERA ARGENTINA S.R.L.",
              "PAN AMERICAN ENERGY LLC, SUCURSAL ARGENTINA","PETROBRAS ENERGIA SA",
              "PAMPA ENERGÍA S.A","TRAFIGURA ARGENTINA S.A.","REFINERIA DEL NORTE SA","OIL COMBUSTIBLES S.A.")
X <- d4[operador %in% privadas & canal_de_comercializacion == "Reventa a otras estaciones de servicio" &
          producto %in% c("Gas Oil Grado 2","Gas Oil Grado 3","Nafta (súper) entre 92 y 95 Ron","Nafta (premium) de más de 95 Ron") &
          precio_sin_impuestos > 0 & !is.na(provincia) & provincia != "N/D" & volumen > 0]
X[, lw := log(precio_sin_impuestos)]
X[, `:=`(q1 = quantile(lw, .01), q99 = quantile(lw, .99)), by = .(producto, anio, mes)]
X <- X[lw >= q1 & lw <= q99]
X[, r := lw - weighted.mean(lw, volumen), by = .(operador, producto, anio, mes)]
X[, `:=`(trimestre = (mes-1) %/% 3 + 1, grupo = fifelse(grepl("Nafta", producto), "nafta", "gasoil"))]
DEL <- X[, .(delta_log = weighted.mean(r, volumen), n_obs = .N, vol_m3 = sum(volumen)), by = .(provincia, anio, trimestre)]
DEL[, delta_prop := exp(delta_log) - 1]
DG <- dcast(X[, .(delta_log = weighted.mean(r, volumen)), by = .(provincia, anio, trimestre, grupo)],
            provincia + anio + trimestre ~ grupo, value.var = "delta_log")
setnames(DG, c("gasoil","nafta"), c("delta_log_gasoil","delta_log_nafta"))
DEL <- merge(DEL, DG, by = c("provincia","anio","trimestre"), all.x = TRUE)
setcolorder(DEL, c("provincia","anio","trimestre","delta_log","delta_prop","delta_log_nafta","delta_log_gasoil","n_obs","vol_m3"))
fwrite(DEL[order(anio, trimestre, provincia)], fs::path(DIR, "covar_escalon_provincial.csv"), bom = TRUE)
cat("[covar_escalon_provincial.csv]", nrow(DEL), "rows\n")

# 8. Check: wholesale price of the private refiners above cU ----
# The implied wholesale margin, w/cU - 1, should be positive (super gasoline and
# grade 2 diesel). It validates the crude price layer without estimating the
# demand model.
Wp <- X[producto %in% c("Nafta (súper) entre 92 y 95 Ron","Gas Oil Grado 2"),
        .(w_ars_l = weighted.mean(precio_sin_impuestos, volumen)), by = .(anio, mes, grupo)]
C3 <- merge(Wp, L[, .(anio, mes, grupo = grupo_grado, cu_ars_l)], by = c("anio","mes","grupo"))
C3[, margen_pct := 100*(w_ars_l/cu_ars_l - 1)]
cat("\nImplied wholesale margin of the private refiners (w/cU - 1, %)\n")
print(dcast(C3[, .(mg = round(mean(margen_pct),1)), by = .(anio, grupo)], anio ~ grupo, value.var = "mg"))
neg <- C3[margen_pct < 0]
cat("grade-months with a negative margin:", nrow(neg), "of", nrow(C3), "|",
    "years:", paste(unique(neg[, paste0(anio)]), collapse = " "), "\n")
fwrite(C3, fs::path(RAWC, "chequeo_c3_margen_privadas.csv"), bom = TRUE)
cat("\nDone.\n")

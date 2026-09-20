# 09_market_data.R
# Downloads and builds the market-level variables the demand and supply models
# need: population, wages and employment by department, national price and
# exchange rate series, the downstream tables of the Energy Secretariat,
# household survey moments and the upstream unit cost.
#
# Input:  public sources, downloaded by the script itself and cached under
#         DIR_COVAR; some parts also read files written by earlier parts
# Output: one covar_*.csv per block in DIR_COVAR
#
# Parts 1 and 2 build the department-level series, and part 2 extends part 1,
# so they run in that order. Part 3 reads the dictionary part 1 writes, part 6
# reads part 5, and part 8 reads parts 5 and 6.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
  library(pdftools)
  library(readxl)
})

source("code/00_config.R")

DIR <- DIR_COVAR
options(timeout = 120)

# 1. Wages and employment by department ----

# Mean wage and registered employment by department and month. Demand-side
# covariate for the BLP model (heterogeneity in price sensitivity).
#
# Input:  CEP-XXI "Datos por departamento" CSVs (downloaded to raw_cep/ if missing)
# Output: covar_ingreso_empleo_depto.csv,
#         cep_diccionario_depto.csv (department dictionary, used by part 3)
#
# Source: CEP-XXI (Ministry of Production), built from SIPA/AFIP records.
#   https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/
#   salarios/w_mean_depto_total.csv, w_mean_depto_priv.csv    mean wage, pesos per month
#   puestos/puestos_depto_total.csv, puestos_depto_priv.csv   registered jobs
#   diccionario_cod_depto.csv                                 INDEC code -> name
#
# Coverage: 505 of the 510 departments in the dictionary, monthly from 2014-01 to
# 2023-11 (nearly balanced panel). The five missing departments are almost
# unpopulated (Gastre, Lihuel Calel, ...). Wages are nominal, so they have to be
# deflated by the CPI for comparisons over time.

dir.create(fs::path(DIR,"raw_cep"), showWarnings=FALSE, recursive=TRUE)
BASE <- "https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento"
files <- c(wt="salarios/w_mean_depto_total.csv", wp="salarios/w_mean_depto_priv.csv",
           pt="puestos/puestos_depto_total.csv", pp="puestos/puestos_depto_priv.csv",
           dic="diccionario_cod_depto.csv")
loc <- setNames(fs::path(DIR,"raw_cep", basename(files)), names(files))
for (k in names(files)) if (!fs::file_exists(loc[k])) {
  cat("downloading", basename(files[k]), "...\n")
  download.file(paste0(BASE,"/",files[k]), loc[k], mode="wb", quiet=TRUE)
}

rd <- function(k) fread(loc[k], encoding="UTF-8")
wt <- rd("wt")
wp <- rd("wp")
pt <- rd("pt")
pp <- rd("pp")
dic <- rd("dic")
setnames(wt,"w_mean","w_mean_total")
setnames(wp,"w_mean","w_mean_priv")
setnames(pt,"puestos","puestos_total")
setnames(pp,"puestos","puestos_priv")

key <- c("fecha","codigo_departamento_indec","id_provincia_indec")
D <- Reduce(function(a,b) merge(a,b,by=key,all=TRUE),
            list(wt, wp[, c(key,"w_mean_priv"), with=FALSE],
                     pt[, c(key,"puestos_total"), with=FALSE],
                     pp[, c(key,"puestos_priv"), with=FALSE]))
D <- merge(D, dic, by=c("codigo_departamento_indec","id_provincia_indec"), all.x=TRUE)

# CEP codes cells suppressed for statistical confidentiality as -99: set to NA
valcols <- c("w_mean_total","w_mean_priv","puestos_total","puestos_priv")
for (cc in valcols) set(D, i = which(D[[cc]] == -99), j = cc, value = NA)
D <- D[!is.na(codigo_departamento_indec)]                 # drops the CEP "not distributed" row, which has no code
D[, fecha := as.Date(fecha)][, `:=`(anio=as.integer(format(fecha,"%Y")), mes=as.integer(format(fecha,"%m")))]
setcolorder(D, c("fecha","anio","mes","codigo_departamento_indec","id_provincia_indec",
                 "nombre_provincia_indec","nombre_departamento_indec",
                 "w_mean_total","w_mean_priv","puestos_total","puestos_priv"))
setorder(D, codigo_departamento_indec, fecha)
fwrite(D, fs::path(DIR,"covar_ingreso_empleo_depto.csv"), bom=TRUE)
fwrite(dic, fs::path(DIR,"cep_diccionario_depto.csv"), bom=TRUE)   # read by part 3
cat("[covar_ingreso_empleo_depto.csv]", nrow(D), "rows |", uniqueN(D$codigo_departamento_indec),
    "departments |", as.character(min(D$fecha)), "to", as.character(max(D$fecha)), "\n")
cat("NA after recoding -99:", paste(valcols, sapply(valcols, function(c) sum(is.na(D[[c]]))), collapse=" | "), "\n")

# 2. Wages extended to 2004-2024 ----

# Extends the department wage series to the whole sample period, 2004-01 to
# 2024-12, with the wage series published by OEDE (Ministry of Labor).
#
# Input:  covar_ingreso_empleo_depto.csv (from part 1, not modified),
#         cep_diccionario_depto.csv, two OEDE workbooks (downloaded if missing)
# Output: covar_ingreso_empleo_depto_ext.csv
#
# The CEP department series only covers 2014-01 to 2023-11. The rest is filled
# with the growth, never the level, of the OEDE series, which measure gross total
# pay of registered private-sector jobs:
#   - 2004-2013: backcast with the monthly provincial series (starts in January
#     1995), with the level anchored to the department/province ratio of 2014.
#     Buenos Aires is split the way OEDE reports it: GBA (the 24 partidos, as
#     departments are called in that province, of Greater Buenos Aires) and the
#     rest of the province. CABA uses "Capital Federal".
#   - 2023-12 to 2024-12: the CEP level is chained with the growth of the OEDE
#     department series (starts in January 2019 and carries the INDEC code),
#     falling back to the provincial series when the department is not there.
#   - Curacó (La Pampa, 3 stations) is not in CEP: it gets the provincial series
#     for the whole period, with the level scaled to its OEDE department series.
# The assumption is that provincial private-sector wage growth is close to that
# of the department total. The column fuente_w flags the origin of each row.
#
# Employment (puestos_*) is not extended and stays NA outside 2014-2023: the
# provincial employment series is quarterly and the wage is the main use here.

options(timeout = 240)

CEP  <- fs::path(DIR, "covar_ingreso_empleo_depto.csv")
XP   <- fs::path(DIR, "raw_oede", "oede_prov_remun_mensual.xlsx")
XD   <- fs::path(DIR, "raw_oede", "oede_depto_empleo_remun.xlsx")
dir.create(fs::path(DIR,"raw_oede"), showWarnings=FALSE, recursive=TRUE)
URLS <- c("https://www.argentina.gob.ar/sites/default/files/provinciales_serie_remuneraciones_mensual_2dig_8.xlsx",
          "https://www.argentina.gob.ar/sites/default/files/departamento_serie_empleo_remuneraciones_3.xlsx")
for (i in 1:2) {
  f <- c(XP,XD)[i]
  if (!fs::file_exists(f)) {
    cat("downloading", basename(f), "...\n")
    download.file(URLS[i], f, mode="wb", quiet=TRUE)
  }
}

norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))   # upper case, no accents
ser2date <- function(s) as.Date(as.numeric(s), origin="1899-12-30")   # Excel serial number -> Date
mfloor <- function(d) as.Date(format(d, "%Y-%m-01"))                  # first day of the month

# 2.1 CEP series (2014-01 to 2023-11) ----
cep <- fread(CEP, encoding="UTF-8")
cep[, fecha := as.Date(fecha)]
cat("CEP:", nrow(cep), "rows |", uniqueN(cep$codigo_departamento_indec), "departments\n")

# 2.2 OEDE provincial monthly series ----
# Sheet "Total": the header row (first cell "Provincia", row 5) holds the dates
# as Excel serial numbers, with one row per series below it.
raw <- suppressMessages(as.data.table(read_excel(XP, sheet="Total", col_names=FALSE)))
hdr <- which(raw[[1]] == "Provincia")[1]
fechas <- mfloor(ser2date(unlist(raw[hdr, -1])))
pv <- raw[(hdr+1):.N]
# Cut at the first empty row so that only the table of provinces is kept. What
# comes below it (source line, notes or more tables, depending on the version of
# the file) is left out. The prov_key filter further down also drops the
# national total and any row that is not a province.
stop_at <- which(is.na(pv[[1]]) | trimws(pv[[1]]) == "")[1]
if (!is.na(stop_at)) pv <- pv[1:(stop_at-1)]
PV <- rbindlist(lapply(seq_len(nrow(pv)), function(i)
  data.table(serie = norm(pv[[1]][i]), fecha = fechas,
             w_prov = suppressWarnings(as.numeric(unlist(pv[i, -1]))))))
PV <- PV[!is.na(fecha) & !is.na(w_prov)]
# Series key: INDEC province id, with Buenos Aires split into GBA and the rest
prov_key <- c("GRAN BUENOS AIRES"="GBA","CAPITAL FEDERAL"="2","BUENOS AIRES"="BA_RESTO",
  "CATAMARCA"="10","CORDOBA"="14","CORRIENTES"="18","CHACO"="22","CHUBUT"="26",
  "ENTRE RIOS"="30","FORMOSA"="34","JUJUY"="38","LA PAMPA"="42","LA RIOJA"="46",
  "MENDOZA"="50","MISIONES"="54","NEUQUEN"="58","RIO NEGRO"="62","SALTA"="66",
  "SAN JUAN"="70","SAN LUIS"="74","SANTA CRUZ"="78","SANTA FE"="82",
  "SANTIAGO DEL ESTERO"="86","TUCUMAN"="90","TIERRA DEL FUEGO"="94")
PV[, skey := prov_key[serie]]
PV <- PV[!is.na(skey)]
stopifnot(!anyDuplicated(PV[, .(skey, fecha)]))     # one value per series-month (no stacked tables)
cat("OEDE provincial:", uniqueN(PV$skey), "series |",
    as.character(min(PV$fecha)), "to", as.character(max(PV$fecha)), "\n")

# 2.3 OEDE department series ----
# Sheets T7 to T12, one per region. The header row starts with "Departamento" and
# holds the dates; the columns are department, 5-digit INDEC code and province,
# followed by the monthly values.
OD <- rbindlist(lapply(paste0("T", 7:12), function(s){
  y <- suppressMessages(as.data.table(read_excel(XD, sheet=s, col_names=FALSE)))
  h <- which(y[[1]] == "Departamento")[1]
  if (is.na(h)) return(NULL)
  fe <- mfloor(ser2date(unlist(y[h, -(1:3)])))
  z <- y[(h+1):.N]
  z <- z[!is.na(z[[2]]) & grepl("^[0-9]{5}$", z[[2]])]
  rbindlist(lapply(seq_len(nrow(z)), function(i)
    data.table(nombre = toupper(trimws(z[[1]][i])), codigo = as.integer(z[[2]][i]), fecha = fe,
               w_oede = suppressWarnings(as.numeric(unlist(z[i, -(1:3)]))))))
}))
OD <- OD[!is.na(fecha) & !is.na(w_oede)]
# Error in the source: OEDE gives Lezama (sheet T8) the code 6266, which belongs
# to Exaltación de la Cruz (sheet T7). The miscoded row is dropped by name, not
# by sheet order, and department-months are required to be unique.
OD <- OD[!(codigo == 6266L & nombre == "LEZAMA")]
stopifnot(!anyDuplicated(OD[, .(codigo, fecha)]))
OD[, nombre := NULL]
cat("OEDE department:", uniqueN(OD$codigo), "departments |",
    as.character(min(OD$fecha)), "to", as.character(max(OD$fecha)), "\n")

# 2.4 Provincial series key of each department ----
# The 24 GBA partidos are identified by name; the other departments of Buenos
# Aires province go with the "rest of Buenos Aires" series.
GBA24 <- norm(c("Almirante Brown","Avellaneda","Berazategui","Esteban Echeverría","Ezeiza",
  "Florencio Varela","General San Martín","Hurlingham","Ituzaingó","José C. Paz","La Matanza",
  "Lanús","Lomas de Zamora","Malvinas Argentinas","Merlo","Moreno","Morón","Quilmes",
  "San Fernando","San Isidro","San Miguel","Tigre","Tres de Febrero","Vicente López"))
deptos <- unique(cep[, .(codigo_departamento_indec, id_provincia_indec,
                         nombre_provincia_indec, nombre_departamento_indec)])
deptos[, dk := norm(nombre_departamento_indec)]
gba_codes <- deptos[id_provincia_indec == 6 & dk %in% GBA24, codigo_departamento_indec]
cat("GBA partidos identified:", length(gba_codes), "of 24\n")
stopifnot(length(gba_codes) == 24L)
deptos[, skey := fifelse(id_provincia_indec == 6,
                         fifelse(codigo_departamento_indec %in% gba_codes, "GBA", "BA_RESTO"),
                         as.character(id_provincia_indec))]

# 2.5 2004-2013: backcast with the provincial series ----
# Anchor: ratio of the department wage to its provincial series, 2014 averages.
anc <- cep[anio == 2014, .(w_tot14 = mean(w_mean_total, na.rm=TRUE),
                           w_prv14 = mean(w_mean_priv,  na.rm=TRUE),
                           n_prv14 = sum(!is.na(w_mean_priv))), by=codigo_departamento_indec]
pv14 <- PV[format(fecha,"%Y") == "2014", .(w_prov14 = mean(w_prov)), by=skey]
anc <- merge(anc, deptos[, .(codigo_departamento_indec, skey)], by="codigo_departamento_indec")
anc <- merge(anc, pv14, by="skey")
anc[, r_tot := w_tot14/w_prov14]
# The private-sector anchor requires at least 6 non-suppressed months in 2014:
# a mean over 2 months that misses the SAC (the half-yearly bonus) is off by
# about 12%, as happens in Ullum. Otherwise the ratio of the total is used.
anc[, r_prv := fifelse(n_prv14 >= 6L & is.finite(w_prv14), w_prv14/w_prov14, r_tot)]
mpre <- PV[fecha >= as.Date("2004-01-01") & fecha <= as.Date("2013-12-01")]
pre <- anc[mpre, on="skey", allow.cartesian=TRUE]
pre <- pre[, .(codigo_departamento_indec, fecha,
               w_mean_total = round(r_tot * w_prov), w_mean_priv = round(r_prv * w_prov),
               fuente_w = "retropolado_oede_prov")]

# 2.6 2023-12 to 2024-12: chain the CEP level with OEDE growth ----
# Anchor: the last month with a non-suppressed wage in each department, from
# 2019 on (the start of the OEDE department series, so that it can be chained).
# It is 2023-11 for nearly every department; those with long suppressed spells
# are chained from further back (Chalileo from 2021-12, 9 de Julio in San Juan
# from 2020-05).
cepl <- cep[fecha >= as.Date("2019-01-01") & fecha <= as.Date("2023-11-01") & !is.na(w_mean_total)]
base_lvl <- cepl[order(fecha), .SD[.N], by=codigo_departamento_indec][
              , .(codigo_departamento_indec, fbase = fecha, w_tot_b = w_mean_total, w_prv_b = w_mean_priv)]
mpost <- seq(as.Date("2023-12-01"), as.Date("2024-12-01"), by="month")
post <- base_lvl[, cbind(.SD[rep(1:.N, each=length(mpost))], fecha = rep(mpost, times=nrow(base_lvl)))]
# OEDE department growth relative to each department's anchor month
post <- merge(post, OD[, .(codigo_departamento_indec = codigo, fbase = fecha, w_ob = w_oede)],
              by=c("codigo_departamento_indec","fbase"), all.x=TRUE)
post <- merge(post, OD[, .(codigo_departamento_indec = codigo, fecha, w_ot = w_oede)],
              by=c("codigo_departamento_indec","fecha"), all.x=TRUE)
post[, g := w_ot / w_ob]
# Provincial fallback, same anchor month
post <- merge(post, deptos[, .(codigo_departamento_indec, skey)], by="codigo_departamento_indec", all.x=TRUE)
post <- merge(post, PV[, .(skey, fbase = fecha, wp_b = w_prov)], by=c("skey","fbase"), all.x=TRUE)
post <- merge(post, PV[, .(skey, fecha, wp_t = w_prov)], by=c("skey","fecha"), all.x=TRUE)
post[, g_prov := wp_t / wp_b]
post[, gg := fifelse(!is.na(g), g, g_prov)]
post[, fuente_w := fifelse(!is.na(g), "extendido_oede_depto", "extendido_oede_prov")]
post <- post[!is.na(gg), .(codigo_departamento_indec, fecha,
              w_mean_total = round(w_tot_b * gg), w_mean_priv = round(w_prv_b * gg), fuente_w)]

# 2.7 Curacó (La Pampa, not in CEP): imputed from the provincial series, 2004-2024 ----
dic <- fread(fs::path(DIR, "cep_diccionario_depto.csv"), encoding="UTF-8")
cur <- dic[norm(nombre_departamento_indec) == "CURACO", codigo_departamento_indec]
if (length(cur) == 1L) {
  mall <- PV[skey == "42" & fecha >= as.Date("2004-01-01") & fecha <= as.Date("2024-12-01")]
  # Level anchored on the OEDE department series of Curacó: the plain provincial
  # series understates it by about 35% (Curacó pays above the La Pampa average)
  ov <- merge(OD[codigo == cur, .(fecha, w_oede)], PV[skey == "42", .(fecha, w_prov)], by="fecha")
  r_cur <- if (nrow(ov) >= 12) mean(ov$w_oede / ov$w_prov) else 1
  cat("Curacó: OEDE department/provincial ratio =", round(r_cur, 3), "(", nrow(ov), "months of overlap )\n")
  curd <- mall[, .(codigo_departamento_indec = cur, fecha,
                   w_mean_total = round(r_cur * w_prov), w_mean_priv = round(r_cur * w_prov),
                   fuente_w = "imputado_prov")]
} else curd <- NULL

# 2.8 Assemble: CEP (fuente_w = "cep"), backcast, extension and Curacó ----
cepx <- cep[, .(codigo_departamento_indec, fecha, w_mean_total, w_mean_priv,
                puestos_total, puestos_priv, fuente_w = "cep")]
ext <- rbindlist(list(cepx, pre, post, curd), fill=TRUE)
ext <- merge(ext, deptos[, .(codigo_departamento_indec, id_provincia_indec,
             nombre_provincia_indec, nombre_departamento_indec)],
             by="codigo_departamento_indec", all.x=TRUE)
# Names for Curacó, which is not in `deptos` because it has no CEP data
if (!is.null(curd)) ext[codigo_departamento_indec == cur & is.na(nombre_departamento_indec),
    `:=`(id_provincia_indec = 42L, nombre_provincia_indec = "La Pampa",
         nombre_departamento_indec = "Curacó")]
ext[, `:=`(anio = year(fecha), mes = month(fecha))]
setcolorder(ext, c("fecha","anio","mes","codigo_departamento_indec","id_provincia_indec",
                   "nombre_provincia_indec","nombre_departamento_indec",
                   "w_mean_total","w_mean_priv","puestos_total","puestos_priv","fuente_w"))
setorder(ext, codigo_departamento_indec, fecha)
stopifnot(!anyDuplicated(ext[, .(codigo_departamento_indec, fecha)]))
fwrite(ext, fs::path(DIR, "covar_ingreso_empleo_depto_ext.csv"), bom=TRUE)

# 2.9 Report and splice checks ----
cat("\n[covar_ingreso_empleo_depto_ext.csv]", nrow(ext), "rows |",
    uniqueN(ext$codigo_departamento_indec), "departments |",
    as.character(min(ext$fecha)), "to", as.character(max(ext$fecha)), "\n")
print(ext[, .N, by=fuente_w])
cat("\nsplice 2013-12 to 2014-01 (ratio of w_total; about 1, give or take monthly inflation, is fine):\n")
s1 <- merge(ext[fecha==as.Date("2013-12-01"), .(codigo_departamento_indec, a=w_mean_total)],
            ext[fecha==as.Date("2014-01-01"), .(codigo_departamento_indec, b=w_mean_total)],
            by="codigo_departamento_indec")
cat("  median b/a:", round(median(s1$b/s1$a, na.rm=TRUE),3),
    "| p5:", round(quantile(s1$b/s1$a,.05,na.rm=TRUE),3),
    "| p95:", round(quantile(s1$b/s1$a,.95,na.rm=TRUE),3), "\n")
cat("splice 2023-11 to 2023-12 (ratio of w_total):\n")
s2 <- merge(ext[fecha==as.Date("2023-11-01"), .(codigo_departamento_indec, a=w_mean_total)],
            ext[fecha==as.Date("2023-12-01"), .(codigo_departamento_indec, b=w_mean_total)],
            by="codigo_departamento_indec")
cat("  median b/a:", round(median(s2$b/s2$a, na.rm=TRUE),3),
    "| p5:", round(quantile(s2$b/s2$a,.05,na.rm=TRUE),3),
    "| p95:", round(quantile(s2$b/s2$a,.95,na.rm=TRUE),3), "\n")
cat("\nexample, CABA (code 2000):\n")
print(ext[codigo_departamento_indec==2000 &
          fecha %in% as.Date(c("2004-01-01","2010-01-01","2013-12-01","2014-01-01",
                               "2023-11-01","2023-12-01","2024-12-01")),
          .(fecha, w_mean_total, fuente_w)])

# 3. Population by department ----

# Population by department and year, the market-size covariate of the BLP
# demand model.
#
# Input:  indec_proyeccion_departamentos_10_25.pdf (downloaded if missing),
#         cep_diccionario_depto.csv (written by part 1)
# Output: covar_poblacion_depto.csv
#
# Source: INDEC, "Estimaciones de población por sexo, departamento y año
# calendario 2010-2025" (Serie Análisis Demográfico 38). It is only published as
# a PDF, so the tables are parsed from the text. The series starts in 2010;
# 2004-2009 is backcast with each department's own 2010-2015 growth rate. An
# alternative that is not implemented: the INDEC 2001-2015 series (2001 census
# base), rescaled to match in 2010.
#
# The projection lines up with both censuses: the national total is 40.79 M in
# 2010 (census 40.12 M) and 46.23 M in 2022 (census 46.04 M, +0.4%).
#
# Output columns: codigo_departamento_indec, id_provincia_indec, provincia,
# departamento, anio, poblacion, fuente. Years 2004-2025, about 512 departments.

dir.create(DIR, showWarnings=FALSE, recursive=TRUE)
PDF  <- fs::path(DIR, "indec_proyeccion_departamentos_10_25.pdf")
DICC <- fs::path(DIR, "cep_diccionario_depto.csv")   # written by part 1
URL  <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/proyeccion_departamentos_10_25.pdf"

# Upper case without accents, to match department names across sources
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

if (!fs::file_exists(PDF)) {
  cat("downloading the INDEC PDF...\n")
  download.file(URL, PDF, mode="wb", quiet=TRUE)
}

# 3.1 Parse the PDF ----
# The text is read line by line, keeping track of the current province (one
# table per province, "Cuadro 1.1" to "Cuadro 1.24"), sex block and year header
# (each table shows 2010-2017 and 2018-2025 in separate blocks). Only the "Ambos
# sexos" (both sexes) rows are kept. CABA enters as a single department, from
# its total row; elsewhere the totals and aggregates matched by SKIP are dropped.
t <- pdf_text(PDF)
provs <- c("CABA","Buenos Aires","Catamarca","Chaco","Chubut","Córdoba","Corrientes",
  "Entre Ríos","Formosa","Jujuy","La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
  "Río Negro","Salta","San Juan","San Luis","Santa Cruz","Santa Fe","Santiago del Estero",
  "Tierra del Fuego","Tucumán")
names(provs) <- paste0("1.", 1:24)
SKIP <- "Partido|Departamento|Comuna|Cuadro|sexos|^Total$|provincia de|Gran Buenos Aires|Interior|Veinticuatro|partidos del|Población|estimada"

rows <- list()
prov <- NA_character_
sexo <- NA_character_
years <- NULL
N <- 0L
for (pg in t) for (ln in strsplit(pg, "\n")[[1]]) {
  # Table number -> province
  cm <- regmatches(ln, regexpr("Cuadro 1\\.\\d+", ln))
  if (length(cm)) {
    cn <- sub("Cuadro ", "", cm)
    if (!is.na(provs[cn])) prov <- provs[[cn]]
  }
  if (grepl("Ambos sexos", ln)) sexo <- "A"
  else if (grepl("\\bVarones\\b", ln)) sexo <- "V"
  else if (grepl("\\bMujeres\\b", ln)) sexo <- "M"
  # Year header of the current block
  if (grepl("20\\d\\d\\s+20\\d\\d", ln)) {
    years <- as.integer(regmatches(ln, gregexpr("20\\d\\d", ln))[[1]])
    N <- length(years)
    next
  }
  if (is.na(sexo) || sexo!="A" || is.null(years) || N==0L || is.na(prov)) next
  # Data row: a name followed by exactly N figures, with "." as thousands separator
  rx <- sprintf("^\\s*(.*?)((?:\\s+\\d{1,3}(?:\\.\\d{3})*){%d})\\s*$", N)
  m <- regmatches(ln, regexec(rx, ln))[[1]]
  if (!length(m)) next
  nm <- trimws(m[2])
  keep_total <- (prov=="CABA" && nm=="Total")
  if (prov=="CABA" && !keep_total) next
  if (!keep_total && (nm=="" || grepl(SKIP, nm, ignore.case=TRUE))) next
  v <- as.integer(gsub("\\.", "", strsplit(trimws(m[3]), "\\s+")[[1]]))
  if (length(v)!=N || anyNA(v)) next
  rows[[length(rows)+1]] <- data.table(provincia=prov, departamento=if(keep_total)"CABA" else nm, year=years, pob=v)
}
P <- dcast(unique(rbindlist(rows), by=c("provincia","departamento","year")), provincia+departamento ~ year, value.var="pob")
ycols <- as.character(2010:2025)
stopifnot(all(rowSums(!is.na(P[, ..ycols]))==16))   # every department has all 16 years
cat("parsed:", nrow(P), "departments | national total 2010 =", format(sum(P[["2010"]])), "| 2022 =", format(sum(P[["2022"]])), "\n")

# 3.2 Long format and 2004-2009 backcast ----
# Each department is carried back from 2010 at its own 2010-2015 annual growth rate.
L <- melt(P, id.vars=c("provincia","departamento"), measure.vars=ycols, variable.name="anio", value.name="poblacion")
L[, anio := as.integer(as.character(anio))]
g <- P[, .(provincia, departamento, p10=get("2010"), cagr=(get("2015")/get("2010"))^(1/5)-1)]
cj <- CJ(idx=seq_len(nrow(g)), anio=2004:2009)               # sorted by idx, so cj$anio lines up with g[cj$idx]
bc <- g[cj$idx][, anio := cj$anio][
       , .(provincia, departamento, anio, poblacion=round(p10/(1+cagr)^(2010-anio)))]
stopifnot(nrow(bc)==6L*nrow(g),                              # six backcast years per department
          all(bc[, uniqueN(anio), by=.(provincia,departamento)]$V1==6L))
L <- rbind(L, bc)
setorder(L, provincia, departamento, anio)

# 3.3 Map to INDEC department codes ----
# Names are matched to the CEP dictionary. The 11 departments in `fix` are
# written differently in the PDF (mostly abbreviations) and are coded by hand.
dic <- fread(DICC, encoding="UTF-8")
dic[, `:=`(pk=norm(nombre_provincia_indec), dk=norm(nombre_departamento_indec))]
L[, `:=`(pk=norm(provincia), dk=norm(departamento))]
L <- merge(L, dic[, .(pk, dk, codigo_departamento_indec, id_provincia_indec)], by=c("pk","dk"), all.x=TRUE)
fix <- data.table(
  dk = norm(c("Chascomús","1º de Mayo","Coronel Felipe Varela","General Angel V. Peñaloza","General Juan F. Quiroga",
              "General Ocampo","La Caldera","La Capital","Juan F. Ibarra","Juan B. Alberdi","Río Grande")),
  pk = norm(c("Buenos Aires","Chaco","La Rioja","La Rioja","La Rioja","La Rioja","Salta","San Luis",
              "Santiago del Estero","Tucumán","Tierra del Fuego")),
  cod_fix = c(6217L,22126L,46028L,46056L,46070L,46084L,66077L,74056L,86098L,90042L,94007L))
fix[, idp_fix := as.integer(cod_fix %/% 1000)]
L <- merge(L, fix, by=c("pk","dk"), all.x=TRUE)
L[is.na(codigo_departamento_indec) & !is.na(cod_fix), `:=`(codigo_departamento_indec=cod_fix, id_provincia_indec=idp_fix)]
cat("mapped to an INDEC code:", uniqueN(L[!is.na(codigo_departamento_indec),.(provincia,departamento)]),
    "of", uniqueN(L[,.(provincia,departamento)]), "(Antártida has no code and no relevant population)\n")

out <- L[, .(codigo_departamento_indec, id_provincia_indec, provincia, departamento, anio, poblacion,
             fuente = ifelse(anio>=2010, "INDEC proy 2010-2025", "backcast 2004-2009"))]
setorder(out, provincia, departamento, anio)
fwrite(out, fs::path(DIR, "covar_poblacion_depto.csv"), bom=TRUE)
cat("[covar_poblacion_depto.csv]", nrow(out), "rows |", uniqueN(out[,.(provincia,departamento)]), "departments | 2004-2025\n")

# Every year should show 512 departments and a national total of 39-46 M
chk <- out[, .(deptos=.N, tot=sum(poblacion)), by=anio][order(anio)]
cat("\nnational total by year:\n")
print(chk[anio %in% c(2004,2007,2009,2010,2015,2022,2025)])

# 4. Population anchored to the 2022 census ----

# Variant of the department population series anchored to the 2022 census.
#
# Input:  covar_poblacion_depto.csv (from part 3, not modified),
#         censo2022_vs_proyeccion_depto.csv
# Output: covar_poblacion_depto_censal.csv, with both series (poblacion_proy and
#         poblacion_censal)
#
# The INDEC projection reproduces the 2010 census department by department, but
# by 2022 it departs from the actual census in some of them (projection over
# census: La Matanza +29%, Valcheta +104%; 48 of 511 departments differ by more
# than 15%). Used as market size, it would inflate or deflate market shares there.
#
# The correction works on shares, not on counts: the projections include the
# adjustment for census undercount (about +1.8% over the raw 2010 count), so
# anchoring to raw census counts would mix two concepts. Each department gets
#     factor_d = census share_d in 2022 / projected share_d in 2022,
# which preserves the projected national total, and the factor is phased in:
#     pob_censal(t) = pob_proy(t) * factor_d^e(t),  e(t) = clamp((t - 2010)/12, 0, 1)
# Years 2004-2010 stay as projected (the projection is accurate there), 2022 has
# the census shares and 2023-2025 keep the full correction.
#
# censo2022_vs_proyeccion_depto.csv has the final 2022 census counts, compiled
# from the 24 provincial tables c2022_*_est_c1 of censo.gob.ar and re-indexed to
# the department codes used here (6218 + 6466 -> 6217, 94008 + 94011 -> 94007,
# CABA comunas -> 2000).

pob <- fread(fs::path(DIR, "covar_poblacion_depto.csv"), encoding="UTF-8")
cmp <- fread(fs::path(DIR, "censo2022_vs_proyeccion_depto.csv"), encoding="UTF-8")
stopifnot(nrow(cmp) == 511L, !anyDuplicated(cmp$codigo_departamento_indec))

# Share factor: census share over projected share, both in 2022
tot_cen <- sum(cmp$cen2022)
tot_proy <- sum(cmp$proy2022)
cmp[, factor22 := (cen2022/tot_cen) / (proy2022/tot_proy)]
cat("factor22: median", round(median(cmp$factor22),3),
    "| p5", round(quantile(cmp$factor22,.05),3), "| p95", round(quantile(cmp$factor22,.95),3), "\n")
cat("La Matanza (6427): factor =", round(cmp[codigo_departamento_indec==6427, factor22], 4), "\n")

P <- merge(pob, cmp[, .(codigo_departamento_indec, factor22)],
           by="codigo_departamento_indec", all.x=TRUE)
P[is.na(factor22), factor22 := 1]                       # Antártida (not in the census): unchanged
P[, e := pmin(pmax((anio - 2010)/12, 0), 1)]            # 0 up to 2010, 1 from 2022 on
P[, poblacion_censal := as.integer(round(poblacion * factor22^e))]
setnames(P, "poblacion", "poblacion_proy")
P[, c("e") := NULL]

out <- P[, .(codigo_departamento_indec, id_provincia_indec, provincia, departamento,
             anio, poblacion_proy, poblacion_censal, factor22 = round(factor22, 4), fuente)]
setorder(out, provincia, departamento, anio)
fwrite(out, fs::path(DIR, "covar_poblacion_depto_censal.csv"), bom=TRUE)

# Checks ----
cat("\n[covar_poblacion_depto_censal.csv]", nrow(out), "rows\n")
cat("\nnational total by year (projection vs census-anchored):\n")
print(out[anio %in% c(2004,2010,2016,2022,2024),
          .(proy=sum(poblacion_proy), censal=sum(poblacion_censal)), by=anio][order(anio)])
cat("\nLa Matanza share in 2022: census-anchored =",
    round(100*out[anio==2022 & codigo_departamento_indec==6427, poblacion_censal] /
              out[anio==2022, sum(poblacion_censal)], 3),
    "% | actual census =", round(100*cmp[codigo_departamento_indec==6427, cen2022]/tot_cen, 3), "%\n")
cat("\nexample, La Matanza (6427):\n")
print(out[codigo_departamento_indec==6427 & anio %in% c(2004,2010,2016,2022,2024),
          .(anio, poblacion_proy, poblacion_censal)])
cat("\nsmoothness: max |annual growth| of the census-anchored series (should be below 9%):\n")
g <- out[order(codigo_departamento_indec, anio),
         .(gmax = max(abs(diff(log(poblacion_censal))), na.rm=TRUE)), by=codigo_departamento_indec]
cat("  p99 =", round(100*quantile(g$gmax, .99, na.rm=TRUE),2), "% | max =", round(100*max(g$gmax, na.rm=TRUE),2), "%\n")

# 5. Downstream tables: sales, refining and imports ----

# Aggregates the SESCO downstream tables of the Energy Secretariat: fuel sales,
# refinery runs and foreign trade.
#
# Input:  raw_sesco/ventas_mercado.csv, procesados.csv, subproductos.csv,
#         impoexpo_2010_2015.csv, impoexpo_2016.csv
# Output: covar_sesco_ventas_prov.csv, covar_sesco_ventas_empresa.csv,
#         covar_sesco_refinacion.csv, covar_sesco_comercio_ext.csv
#
# Source: CKAN dataset 5bdc436c, "Refinación y Comercialización (Tablas
# Dinámicas)", at datos.energia.gob.ar. The raw files (about 1.1 GB) sit in
# covariables_mercado/raw_sesco/ and are not downloaded by this script; only the
# small aggregates are written to covariables_mercado/:
#   covar_sesco_ventas_prov.csv     province x product x channel x month (market size
#                                   for the BLP and external check on the volumes)
#   covar_sesco_ventas_empresa.csv  company x province x product x month, gasoline and
#                                   diesel in the retail channel only (shares by brand)
#   covar_sesco_refinacion.csv      refinery x concept x month: inputs processed and
#                                   by-products (cost and supply shifter)
#   covar_sesco_comercio_ext.csv    product x trade type x month, with quantity and
#                                   value; the import unit value is the observed
#                                   import parity c^U of the model

CACHE <- fs::path(DIR, "raw_sesco")   # raw files, kept inside the data tree
stopifnot(dir.exists(CACHE))   # if missing, download the files again from the CKAN dataset above

# 5.1 Sales to the market (sales within the sector excluded), 6.5 M rows ----
V <- fread(fs::path(CACHE,"ventas_mercado.csv"), encoding="UTF-8",
           select=c("anio","mes","empresa","subtipodecomercializacion","producto",
                    "unidad","provincia","cantidad"))
setnames(V, "subtipodecomercializacion", "canal")
cat("sales: rows", format(nrow(V), big.mark="."), "| years", min(V$anio), "-", max(V$anio), "\n")
cat("channels:\n")
print(V[, .N, by=canal][order(-N)])

# (A) province x product x channel x month
A <- V[, .(cantidad = sum(cantidad, na.rm=TRUE)), by=.(anio, mes, provincia, producto, unidad, canal)]
A <- A[cantidad != 0]
fwrite(A, fs::path(DIR,"covar_sesco_ventas_prov.csv"), bom=TRUE)
cat("[covar_sesco_ventas_prov.csv]", nrow(A), "rows\n")

# (B) company x province x product x month: gasoline and diesel grades, retail channel
FOCAL <- unique(V[grepl("^Nafta Grado|^Gasoil Grado", producto), producto])
cat("focal products:", paste(FOCAL, collapse=" | "), "\n")
# Only "Al Público", the retail sales at service stations. It is not the public
# passenger transport channel.
B <- V[producto %in% FOCAL & canal == "Al Público",
       .(cantidad = sum(cantidad, na.rm=TRUE)), by=.(anio, mes, empresa, provincia, producto)]
B <- B[cantidad != 0]
fwrite(B, fs::path(DIR,"covar_sesco_ventas_empresa.csv"), bom=TRUE)
cat("[covar_sesco_ventas_empresa.csv]", nrow(B), "rows |", uniqueN(B$empresa), "companies\n")

# Check 1: national total of gasoline and diesel in the retail channel, by year
cat("\nCheck 1, against the station panel: national total of focal products, retail channel (thousand m3 per year)\n")
v1 <- B[, .(miles_m3 = round(sum(cantidad)/1e3)), by=anio][order(anio)]
print(v1)
cat("(station panel benchmark: 10-17.5 M m3 per year with the outlier filter)\n")

# Check 2: YPF share of gasoline and diesel in the retail channel
cat("\nCheck 2: YPF share by year (%)\n")
v2 <- B[, .(tot = sum(cantidad), ypf = sum(cantidad[grepl("^YPF", empresa)])), by=anio]
v2[, share_ypf := round(100*ypf/tot, 1)]
print(v2[order(anio), .(anio, share_ypf)])
cat("(station panel benchmark: about 53-54% of volume)\n")
rm(V)
invisible(gc())

# 5.2 Refining: inputs processed and by-products, by refinery and month ----
P <- fread(fs::path(CACHE,"procesados.csv"), encoding="UTF-8",
           select=c("anio","mes","empresa","refineria","concepto","cantidadm3"))
S <- fread(fs::path(CACHE,"subproductos.csv"), encoding="UTF-8",
           select=c("anio","mes","empresa","refineria","concepto","cantidadm3"))
P[, bloque := "carga"]
S[, bloque := "subproducto"]
R <- rbind(P, S)[cantidadm3 != 0]
R <- R[, .(cantidad_m3 = sum(cantidadm3)), by=.(anio, mes, empresa, refineria, concepto, bloque)]
fwrite(R, fs::path(DIR,"covar_sesco_refinacion.csv"), bom=TRUE)
cat("\n[covar_sesco_refinacion.csv]", nrow(R), "rows |", uniqueN(R$refineria), "refineries |",
    "years", min(R$anio), "-", max(R$anio), "\n")
# Within "carga" (inputs), concepto is either crude oil, identified by its basin
# ("Cuenca ..."), or another input (intermediate cuts, lubricant bases, biofuels,
# gasoline from other origins). Of the 48 units, 28 are refineries that run crude
# and 20 are blending plants. Crude processed is obtained with
# grepl("Cuenca|Petróleo", concepto); dropping the biofuels alone is not enough.
cat("\ninput concepts:\n")
print(R[bloque=="carga", .N, by=concepto][order(-N)][1:8])
cat("\nCheck 3: top 5 refineries by crude processed in 2019 (thousand m3)\n")
print(R[anio==2019 & bloque=="carga" & grepl("Cuenca|Petr[óo]leo", concepto),
        .(miles_m3 = round(sum(cantidad_m3)/1e3)), by=refineria][order(-miles_m3)][1:5])
rm(P,S)
invisible(gc())

# 5.3 Foreign trade: quantity and value -> unit value (observed import parity) ----
CE <- rbind(
  fread(fs::path(CACHE,"impoexpo_2010_2015.csv"), encoding="UTF-8",
        select=c("anio","mes","tipodecomercializacion","producto","unidad","cantidad","monto")),
  fread(fs::path(CACHE,"impoexpo_2016.csv"), encoding="UTF-8",
        select=c("anio","mes","tipodecomercializacion","producto","unidad","cantidad","monto")))
setnames(CE, "tipodecomercializacion", "tipo")
E <- CE[, .(cantidad = sum(cantidad, na.rm=TRUE), monto_usd = sum(monto, na.rm=TRUE)),
        by=.(anio, mes, tipo, producto, unidad)]
E <- E[cantidad != 0 | monto_usd != 0]
E[cantidad > 0, valor_unitario := round(monto_usd / cantidad, 2)]
fwrite(E, fs::path(DIR,"covar_sesco_comercio_ext.csv"), bom=TRUE)
cat("\n[covar_sesco_comercio_ext.csv]", nrow(E), "rows | years", min(E$anio), "-", max(E$anio), "\n")
cat("\nCheck 4: import unit value of diesel (USD/m3)\n")
g <- E[tipo=="Importación" & grepl("^Gasoil", producto) & cantidad > 1000,
       .(vu = round(weighted.mean(valor_unitario, cantidad, na.rm=TRUE))), by=anio][order(anio)]
print(g)
cat("(expected: it tracks crude; peak in 2022, above 1,000 USD/m3, with the diesel shortage)\n")

# 6. National series: prices, crude and exchange rate ----

# National and international monthly series, 2004-01 to 2024-12: spliced CPI,
# Brent, US spot prices of refined products and the official exchange rate.
#
# Input:  public downloads from the datos.gob.ar series API and FRED (no account
#         or API key needed), cached in raw_series/
# Output: covar_series_nacionales.csv
#
# (1) Spliced CPI, December 2016 = 100. The official CPI is unreliable over
#     2007-2015 (intervention of INDEC), so monthly changes are chained across
#     three indices, as is standard in the literature:
#       2004-01 to 2005-11  INDEC CPI for Greater Buenos Aires (base April 2008)
#       2005-12 to 2016-11  San Luis CPI (DPEyC, base 2003), a reliable provincial index
#       2016-12 to 2024-12  INDEC national CPI (December 2016 = 100)
#     A citable alternative for robustness: CIFRA-CTA "IPC Provincias" (2007-2018).
# (2) Brent spot, USD per barrel (FRED MCOILBRENTEU, from EIA).
# (3) Spot prices of refined products in USD per gallon and per m3 (FRED, from
#     EIA): regular conventional gasoline and No. 2 ULSD diesel, US Gulf Coast
#     (USGC) and New York Harbor (NYH, as a backup). They feed the predicted
#     import parity for 2004-2009, when the parity observed in SESCO does not
#     exist. ULSD starts in June 2006; earlier months are spliced with NYH No. 2
#     heating oil (columns *_emp).
# (4) Official wholesale exchange rate (BCRA Communication A 3500), monthly
#     average from the datos.gob.ar API. It averages over calendar days, with
#     weekends repeating the Friday value; a business-day average would have to
#     be built from the BCRA file com3500.xls.

RAW  <- fs::path(DIR, "raw_series")
dir.create(RAW, showWarnings=FALSE, recursive=TRUE)
mfloor <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))

dl <- function(url, dest) if (!fs::file_exists(dest)) download.file(url, dest, mode="wb", quiet=TRUE)

# 6.1 CPI: the four indices in a single API call ----
U_IPC <- paste0("https://apis.datos.gob.ar/series/api/series/?ids=",
  "96.3_ING_2008_M_19,",            # CPI GBA, base April 2008 (1993-2013; reliable only up to 2006)
  "197.1_NIVEL_GENERAL_2014_0_13,", # CPI San Luis, base 2003 (from 2005-10)
  "193.1_NIVEL_GENERAL_JULI_0_13,", # CPI CABA (from 2012-07; for reference only)
  "148.3_INIVELNAL_DICI_M_26",      # national CPI (December 2016 = 100)
  "&format=csv&limit=1000&start_date=2004-01-01&header=ids")
dl(U_IPC, fs::path(RAW, "ipc_tramos.csv"))
ipc <- fread(fs::path(RAW, "ipc_tramos.csv"), encoding="UTF-8")
setnames(ipc, c("mes","gba","sanluis","caba","nacional"))
ipc[, mes := as.Date(mes)]

SLd16 <- ipc[mes==as.Date("2016-12-01"), sanluis]     # 1350.48
GBAd06 <- ipc[mes==as.Date("2006-12-01"), gba]        # 89.1559
stopifnot(is.finite(SLd16), is.finite(GBAd06))
ipc[, ipc_empalmado := fifelse(
      mes >= as.Date("2016-12-01"), nacional,
      fifelse(mes >= as.Date("2005-12-01"), 100 * sanluis / SLd16,     # San Luis segment (2005-12 to 2016-11)
              NA_real_))]
ipc_d05 <- ipc[mes==as.Date("2005-12-01"), ipc_empalmado]              # level at the end of the GBA segment
GBA_d05 <- ipc[mes==as.Date("2005-12-01"), gba]
ipc[mes < as.Date("2005-12-01"), ipc_empalmado := ipc_d05 * gba / GBA_d05]
ipc[, fuente_ipc := fifelse(mes >= as.Date("2016-12-01"), "indec_nacional",
                    fifelse(mes >= as.Date("2005-12-01"), "san_luis_encadenado",
                            "gba_indec_encadenado"))]
# The San Luis segment starts in December 2005 (its series begins in October
# 2005), so the GBA/San Luis splice falls well before the intervention.

# 6.2 Brent, monthly (FRED, USD/bbl) ----
U_BR <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MCOILBRENTEU&cosd=2004-01-01&coed=2024-12-31"
dl(U_BR, fs::path(RAW, "brent_fred.csv"))
br <- fread(fs::path(RAW, "brent_fred.csv"), encoding="UTF-8")
setnames(br, c("mes","brent_usd_bbl"))
br[, mes := mfloor(mes)]

# 6.3 Spot prices of refined products, USD/gal -> USD/m3 (FRED, from EIA) ----
# FRED series:
#   MGASUSGULF    Conventional Gasoline Prices: U.S. Gulf Coast, Regular        (from 2004-01)
#   MDFUELUSGULF  Ultra-Low-Sulfur No. 2 Diesel Fuel Prices: U.S. Gulf Coast    (from 2006-06)
#   MGASNYH       Conventional Gasoline Prices: New York Harbor, Regular        (from 2004-01)
#   MDFUELNYH     Ultra-Low-Sulfur No. 2 Diesel Fuel Prices: New York Harbor    (from 2006-06)
#   MHOILNYH      No. 2 Heating Oil Prices: New York Harbor                     (from 1986-06)
# The M* series are the monthly averages that EIA publishes of the daily D*
# series (DGASUSGULF, DDFUELUSGULF, DGASNYH, DDFUELNYH) and are used as they come.
# 1 US gallon = 0.003785411784 m3.
#
# Diesel before June 2006: EIA has no ULSD quotes before 2006 (the earlier spot
# was "No. 2 diesel low sulfur", which is not on FRED). For 2004-01 to 2006-05,
# NYH heating oil is scaled by the mean ULSD/heating oil ratio over the first 12
# months of overlap (2006-06 to 2007-05). Heating oil and No. 2 diesel are the
# same middle-distillate cut and differ only in sulfur content, so they move
# almost one for one. The result goes to the columns *_emp and fuente_gasoil_emp.
GAL_M3  <- 0.003785411784
REF_IDS <- c(nafta_usgulf="MGASUSGULF", gasoil_usgulf="MDFUELUSGULF",
             nafta_nyh="MGASNYH",       gasoil_nyh="MDFUELNYH",
             heating_nyh="MHOILNYH")
ref <- data.table(mes = seq(as.Date("2004-01-01"), as.Date("2024-12-01"), by="month"))
for (nm in names(REF_IDS)) {
  id <- REF_IDS[[nm]]
  U  <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=2004-01-01&coed=2024-12-31", id)
  f  <- fs::path(RAW, sprintf("%s_fred.csv", nm))
  dl(U, f)
  x <- fread(f, encoding="UTF-8", na.strings=c(".",""))      # FRED codes missing values as "."
  stopifnot(ncol(x)==2L, names(x)[2]==id)
  setnames(x, c("mes", paste0(nm, "_usd_gal")))
  x[, mes := mfloor(mes)]
  x <- x[, .(v = mean(get(paste0(nm,"_usd_gal")), na.rm=TRUE)), by=mes]   # in case the series comes at daily frequency
  x[!is.finite(v), v := NA_real_]
  setnames(x, "v", paste0(nm, "_usd_gal"))
  ref <- merge(ref, x, by="mes", all.x=TRUE)
}
# Splice USGC and NYH diesel with NYH heating oil for 2004-01 to 2006-05
for (mk in c("usgulf","nyh")) {
  g  <- paste0("gasoil_", mk, "_usd_gal")
  ini <- ref[!is.na(get(g)), min(mes)]
  ov  <- ref[!is.na(get(g)) & !is.na(heating_nyh_usd_gal)][order(mes)][1:12]   # first 12 months of overlap
  k   <- mean(ov[[g]] / ov$heating_nyh_usd_gal)
  stopifnot(is.finite(k), abs(k-1) < 0.15)     # the ULSD/heating oil ratio should be within 15% of 1
  ref[, paste0("gasoil_", mk, "_emp_usd_gal") :=
        fifelse(!is.na(get(g)), get(g), round(k * heating_nyh_usd_gal, 3))]
  cat(sprintf("gasoil_%s: ULSD from %s; ULSD/heating oil ratio (12m) = %.4f\n", mk, ini, k))
}
ref[, fuente_gasoil_emp := fifelse(!is.na(gasoil_usgulf_usd_gal), "ulsd_eia",
                           fifelse(!is.na(heating_nyh_usd_gal), "heating_oil_nyh_escalado", NA_character_))]
for (cn in grep("_usd_gal$", names(ref), value=TRUE))
  ref[, sub("_usd_gal$", "_usd_m3", cn) := round(get(cn) / GAL_M3, 2)]
ref_cols <- c("mes",
  "nafta_usgulf_usd_gal","nafta_usgulf_usd_m3","gasoil_usgulf_usd_gal","gasoil_usgulf_usd_m3",
  "nafta_nyh_usd_gal","nafta_nyh_usd_m3","gasoil_nyh_usd_gal","gasoil_nyh_usd_m3",
  "heating_nyh_usd_gal","heating_nyh_usd_m3",
  "gasoil_usgulf_emp_usd_gal","gasoil_usgulf_emp_usd_m3","gasoil_nyh_emp_usd_gal","gasoil_nyh_emp_usd_m3",
  "fuente_gasoil_emp")
ref <- ref[, ..ref_cols]

# 6.4 Official exchange rate A 3500, monthly average (datos.gob.ar mirror of BCRA) ----
U_TC <- paste0("https://apis.datos.gob.ar/series/api/series/?ids=175.1_DR_REFE500_0_0_25",
  "&format=csv&collapse=month&collapse_aggregation=avg&start_date=2004-01&end_date=2024-12&limit=5000")
dl(U_TC, fs::path(RAW, "tc_a3500_mensual.csv"))
tc <- fread(fs::path(RAW, "tc_a3500_mensual.csv"), encoding="UTF-8")
setnames(tc, c("mes","tc_a3500_prom"))
tc[, mes := as.Date(mes)]

# 6.5 Assemble 2004-01 to 2024-12 ----
out <- data.table(mes = seq(as.Date("2004-01-01"), as.Date("2024-12-01"), by="month"))
out <- Reduce(function(a,b) merge(a,b,by="mes",all.x=TRUE),
              list(out, ipc[, .(mes, ipc_empalmado = round(ipc_empalmado,3), fuente_ipc,
                                ipc_gba=gba, ipc_sanluis=sanluis, ipc_caba=caba, ipc_nacional=nacional)],
                   br, tc, ref))
out[, ipc_2004_100 := round(100 * ipc_empalmado / out[mes==as.Date("2004-01-01"), ipc_empalmado], 2)]
# CPI, Brent and exchange rate columns first, refined products at the end
setcolorder(out, c("mes","ipc_empalmado","fuente_ipc","ipc_gba","ipc_sanluis","ipc_caba","ipc_nacional",
                   "brent_usd_bbl","tc_a3500_prom","ipc_2004_100"))
stopifnot(nrow(out)==252L, !anyNA(out$ipc_empalmado), !anyNA(out$brent_usd_bbl), !anyNA(out$tc_a3500_prom),
          !anyNA(out$nafta_usgulf_usd_gal), !anyNA(out$gasoil_usgulf_emp_usd_gal),
          !anyNA(out[mes >= as.Date("2006-06-01"), gasoil_usgulf_usd_gal]))
fwrite(out, fs::path(DIR, "covar_series_nacionales.csv"), bom=TRUE)

# 6.6 Checks against known milestones ----
cat("[covar_series_nacionales.csv]", nrow(out), "months | columns:", paste(names(out), collapse=", "), "\n")
infl <- function(y) round(100*(out[mes==as.Date(sprintf("%d-12-01",y)), ipc_empalmado] /
                               out[mes==as.Date(sprintf("%d-12-01",y-1)), ipc_empalmado] - 1), 1)
cat("\nDecember-to-December inflation (compare with known figures):\n")
for (y in c(2005, 2007, 2010, 2014, 2016, 2019, 2023, 2024))
  cat(sprintf("  %d: %5.1f%%\n", y, infl(y)))
cat("\nsplices (m/m change around the splice month, should look normal):\n")
for (m in c("2005-12-01","2006-01-01","2016-12-01","2017-01-01")) {
  i <- which(out$mes==as.Date(m))
  cat(sprintf("  %s: %+.2f%%  [%s]\n", m, 100*(out$ipc_empalmado[i]/out$ipc_empalmado[i-1]-1), out$fuente_ipc[i]))
}
cat("\nBrent and exchange rate milestones:\n")
cat("  Brent Jul-2008 (peak):", out[mes==as.Date("2008-07-01"), brent_usd_bbl], "USD |",
    "Apr-2020 (COVID):", out[mes==as.Date("2020-04-01"), brent_usd_bbl], "USD\n")
cat("  FX Nov-2015:", out[mes==as.Date("2015-11-01"), tc_a3500_prom], "| Dec-2015 (capital controls lifted):",
    out[mes==as.Date("2015-12-01"), tc_a3500_prom], "\n")
cat("  FX Nov-2023:", out[mes==as.Date("2023-11-01"), tc_a3500_prom], "| Dec-2023 (devaluation):",
    out[mes==as.Date("2023-12-01"), tc_a3500_prom], "\n")

cat("\nrefined products (USD/gal): coverage and NA\n")
for (cn in grep("_usd_gal$", names(out), value=TRUE)) {
  ok <- out[!is.na(get(cn))]
  cat(sprintf("  %-28s %s to %s | NA: %3d | min %.3f  max %.3f\n", cn,
              format(min(ok$mes), "%Y-%m"), format(max(ok$mes), "%Y-%m"),
              sum(is.na(out[[cn]])), min(ok[[cn]]), max(ok[[cn]])))
}
pk <- function(cn, y) {
  s <- out[year(mes)==y]
  s[which.max(get(cn)), sprintf("%s (%.3f)", format(mes,"%Y-%m"), get(cn))]
}
cat("  USGC gasoline peak 2008:", pk("nafta_usgulf_usd_gal", 2008), "| peak 2022:", pk("nafta_usgulf_usd_gal", 2022), "\n")
cat("  USGC diesel peak 2008:", pk("gasoil_usgulf_usd_gal", 2008), "| peak 2022:", pk("gasoil_usgulf_usd_gal", 2022), "\n")

# External check: USGC gasoline against the import unit value of Nafta Grado 3
# in SESCO. Runs only if part 5 has already written its trade file.
f_ce <- fs::path(DIR, "covar_sesco_comercio_ext.csv")
if (fs::file_exists(f_ce)) {
  ce <- fread(f_ce, encoding="UTF-8")
  ce <- ce[tipo=="Importación" & grepl("^Nafta Grado 3", producto),
           .(mes = as.Date(sprintf("%d-%02d-01", anio, mes)), vu_imp_nafta_usd_m3 = valor_unitario)]
  cmp <- merge(ce, out[, .(mes, nafta_usgulf_usd_m3, brent_usd_bbl)], by="mes")
  cmp <- cmp[is.finite(vu_imp_nafta_usd_m3) & vu_imp_nafta_usd_m3 > 0]
  cat(sprintf("\nUSGC gasoline (USD/m3) vs SESCO import unit value of Nafta Grado 3 (%d months, %s..%s):\n",
              nrow(cmp), format(min(cmp$mes),"%Y-%m"), format(max(cmp$mes),"%Y-%m")))
  cat(sprintf("  SESCO/USGC ratio: mean %.3f | median %.3f | p10 %.3f | p90 %.3f\n",
              cmp[, mean(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3)], cmp[, median(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3)],
              cmp[, quantile(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3, .1)], cmp[, quantile(vu_imp_nafta_usd_m3/nafta_usgulf_usd_m3, .9)]))
  cat(sprintf("  corr(levels) = %.3f | corr(logs) = %.3f | corr with Brent = %.3f\n",
              cmp[, cor(vu_imp_nafta_usd_m3, nafta_usgulf_usd_m3)], cmp[, cor(log(vu_imp_nafta_usd_m3), log(nafta_usgulf_usd_m3))],
              cmp[, cor(vu_imp_nafta_usd_m3, brent_usd_bbl)]))
}

# 7. Household survey moments ----

# Fuel expenditure by income decile and car ownership by province, from the
# 2017-18 household expenditure survey (ENGHo, INDEC).
#
# Input:  engho2018_gastos.txt, engho2018_hogares.txt (INDEC public microdata,
#         downloaded to raw_engho/ if missing)
# Output: covar_engho_micromomentos.csv, covar_engho_motorizacion.csv
#
# ENGHo records the detailed expenditure of each household (COICOP), its income,
# region and car ownership. Two things are built from it:
#  (1) Micro-moments for the BLP model (as in Petrin, or the micro moments of
#      Conlon and Gortmaker): share of households that buy fuel and budget share
#      of fuel, by decile of per capita income and by region. They discipline the
#      income-price interaction in the GMM without the need for local income data.
#  (2) Car ownership by province, an input for a "physical" market size
#      (population x ownership rate x average consumption), used as a robustness
#      check.
#
# Fuel expenditure is COICOP class "A0722" (fuels and lubricants for personal
# transport equipment). A household owns a car when propauto >= 2: the variable
# is coded 1 = none, 2 = one car, 3 = two or more (checked against the equipment
# module: it reproduces the 40.6% of CABA). Weights are in pondera.
#
# Only the 2017-18 wave is processed; the 2004-05 and 2012-13 waves, needed for a
# motorization series, are still pending (RAR files).

options(timeout = 300)

RAW  <- fs::path(DIR, "raw_engho")
dir.create(RAW, showWarnings=FALSE, recursive=TRUE)
BASE_URL <- "https://www.indec.gob.ar/ftp/cuadros/menusuperior/engho"
for (z in c("engho2018_gastos.zip","engho2018_hogares.zip")) {
  fz <- fs::path(RAW, z)
  if (!fs::file_exists(fz)) {
    download.file(paste0(BASE_URL,"/",z), fz, mode="wb", quiet=TRUE)
    unzip(fz, exdir=RAW)
  }
  ft <- fs::path(RAW, sub("zip$","txt",z))
  if (!fs::file_exists(ft)) unzip(fz, exdir=RAW)
}

G <- fread(fs::path(RAW,"engho2018_gastos.txt"), sep="|", encoding="UTF-8",
           select=c("id","clase","monto"))
H <- fread(fs::path(RAW,"engho2018_hogares.txt"), sep="|", encoding="UTF-8",
           select=c("id","provincia","region","pondera","gastot","ingtoth","ingpch","propauto"))

# Fuel expenditure per household (zero if none is reported)
comb <- G[clase=="A0722", .(gasto_comb = sum(monto)), by=id]
D <- merge(H, comb, by="id", all.x=TRUE)
D[is.na(gasto_comb), gasto_comb := 0]
D <- D[gastot > 0 & ingtoth > 0]

# Weighted deciles of per capita household income
setorder(D, ingpch)
D[, decil := pmin(10L, 1L + as.integer(10*cumsum(pondera)/sum(pondera) - 1e-9))]

# Moments by income decile (MM) and by INDEC region (MR)
MM <- D[, .(nivel = "decil_ingreso_pc", grupo = as.character(decil[1]),
            hogares_muestra = .N,
            pct_compra_combustible = round(100*weighted.mean(gasto_comb>0, pondera), 2),
            share_presupuesto      = round(100*weighted.mean(gasto_comb/gastot, pondera), 3),
            share_si_compra        = round(100*weighted.mean((gasto_comb/gastot)[gasto_comb>0],
                                                             pondera[gasto_comb>0]), 3),
            pct_tiene_auto         = round(100*weighted.mean(propauto>=2, pondera), 2)), by=decil]
MR <- D[, .(nivel = "region_indec", grupo = as.character(region[1]),
            hogares_muestra = .N,
            pct_compra_combustible = round(100*weighted.mean(gasto_comb>0, pondera), 2),
            share_presupuesto      = round(100*weighted.mean(gasto_comb/gastot, pondera), 3),
            share_si_compra        = round(100*weighted.mean((gasto_comb/gastot)[gasto_comb>0],
                                                             pondera[gasto_comb>0]), 3),
            pct_tiene_auto         = round(100*weighted.mean(propauto>=2, pondera), 2)), by=region]
OUT <- rbind(MM[, !"decil"], MR[, !"region"])
fwrite(OUT, fs::path(DIR, "covar_engho_micromomentos.csv"), bom=TRUE)

# Share of households with a car, by province
MOT <- D[, .(pct_hogares_con_auto = round(100*weighted.mean(propauto>=2, pondera), 2),
             hogares_muestra = .N, onda = "2017-18"), by=provincia][order(provincia)]
fwrite(MOT, fs::path(DIR, "covar_engho_motorizacion.csv"), bom=TRUE)

cat("[covar_engho_micromomentos.csv]", nrow(OUT), "rows (10 deciles + 6 regions)\n")
cat("[covar_engho_motorizacion.csv]", nrow(MOT), "provinces | 2017-18 wave\n")
cat("\nchecks: budget share d1 =", MM[decil==1, share_presupuesto],
    "% and d10 =", MM[decil==10, share_presupuesto],
    "% | car ownership d1 =", MM[decil==1, pct_tiene_auto], "% and d10 =", MM[decil==10, pct_tiene_auto], "%\n")

# 8. Upstream unit cost ----

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

RAWC <- fs::path(DIR, "raw_costo")
MAYO <- fs::path(DIR_WHOLESALE, "precios_mayoristas_res1104.csv")
if (!dir.exists(RAWC)) dir.create(RAWC)
GAL_M3 <- 264.172        # gallons per m3
BBL_M3 <- 6.28981        # barrels per m3
DENS_BIODIESEL <- 0.88   # kg/l (FAME), so 1 t = 1136.4 l

# 8.1 Realized crude price: royalties report (Energy Secretariat) ----
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

# 8.2 Crude slate of the YPF refineries (SESCO refining) -> national crude price by month ----
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

# 8.3 National series (Brent, exchange rate, FOB) and YPF refining yield ----
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
# The extract of the 20-F filings also ships with the repository; without it the
# refining cost below stays at the placeholder of 5 USD per barrel.
if (!fs::file_exists(f20)) f20 <- fs::path("data", "20f_ypf_extract.csv")
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

# 8.4 Refining opex (20-F): annual, constant within the year ----
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

# 8.5 Biofuels: mandatory blend x regulated price ----
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

# 8.6 Assemble cU ----
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

# 8.7 Provincial step delta_prov ----
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

# 8.8 Check: wholesale price of the private refiners above cU ----
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


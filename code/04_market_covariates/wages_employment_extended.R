# wages_employment_extended.R
# Extends the department wage series to the whole sample period, 2004-01 to
# 2024-12, with the wage series published by OEDE (Ministry of Labor).
#
# Input:  covar_ingreso_empleo_depto.csv (from wages_employment.R, not modified),
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

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(fs)
})
source("code/00_config.R")
options(timeout = 240)

DIR  <- DIR_COVAR
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

# 1. CEP series (2014-01 to 2023-11) ----
cep <- fread(CEP, encoding="UTF-8")
cep[, fecha := as.Date(fecha)]
cat("CEP:", nrow(cep), "rows |", uniqueN(cep$codigo_departamento_indec), "departments\n")

# 2. OEDE provincial monthly series ----
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

# 3. OEDE department series ----
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

# 4. Provincial series key of each department ----
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

# 5. 2004-2013: backcast with the provincial series ----
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

# 6. 2023-12 to 2024-12: chain the CEP level with OEDE growth ----
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

# 7. Curacó (La Pampa, not in CEP): imputed from the provincial series, 2004-2024 ----
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

# 8. Assemble: CEP (fuente_w = "cep"), backcast, extension and Curacó ----
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

# 9. Report and splice checks ----
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

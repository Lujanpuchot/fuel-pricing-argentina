# wages_employment.R
# Mean wage and registered employment by department and month. Demand-side
# covariate for the BLP model (heterogeneity in price sensitivity).
#
# Input:  CEP-XXI "Datos por departamento" CSVs (downloaded to raw_cep/ if missing)
# Output: covar_ingreso_empleo_depto.csv,
#         cep_diccionario_depto.csv (department dictionary, used by population.R)
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

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})
source("code/00_config.R")
options(timeout = 120)

DIR  <- DIR_COVAR
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
fwrite(dic, fs::path(DIR,"cep_diccionario_depto.csv"), bom=TRUE)   # read by population.R
cat("[covar_ingreso_empleo_depto.csv]", nrow(D), "rows |", uniqueN(D$codigo_departamento_indec),
    "departments |", as.character(min(D$fecha)), "to", as.character(max(D$fecha)), "\n")
cat("NA after recoding -99:", paste(valcols, sapply(valcols, function(c) sum(is.na(D[[c]]))), collapse=" | "), "\n")

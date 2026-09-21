# 12_estimation_sample.R
# Joins the market and station variables onto the product-market file that
# 11_demand_sample.R writes, and reports how much of each variable survives the
# join. The estimation reads what this script saves.
#
# Input:  demanda_blp_sample_<system>.csv, the covar_* files of 09_market_data.R
#         and the station files of 08_station_variables.R
# Output: estimation_sample_<system>.csv in DIR_INTERIM, plus a coverage table
#
# Two joins here are not plumbing and are set as options at the top.
#
# The first is the key. The market covariates are keyed by INDEC department
# code; the panel is keyed by the province and department names the Energy
# Secretariat uses. crosswalk_depto_codigo.csv bridges them, and three codes
# are shared by two departments each, because the INDEC vintage predates the
# splits: Chascomús and Lezama share 6217, Río Grande and Tolhuin share 94007,
# and Laguna Seca is a neighbourhood inside Corrientes Capital, 18021. Left
# alone, both members of each pair would be given the population of the pair,
# and the market size would count the same people twice. SHARED_CODE decides
# what to do about it.
#
# The second is the grain. Station variables are one row per outlet, or per
# outlet and year; a product in this file is a brand and a grade in a department
# and a quarter, which is many outlets. Turning one into the other is an
# average, and which average is a modelling choice: a station that sells ten
# times more should probably count ten times more, but that makes the
# characteristic depend on the quantity the model is trying to explain.
# STATION_AGG holds the choice and the alternatives are worth estimating across.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})

source("code/00_config.R")

DIR_INT <- DIR_INTERIM
DIR_COV <- DIR_COVAR

# Settings ----

PRODUCT_SYSTEM <- "nafta"
SHARED_CODE    <- "collapse"   # "collapse": one market per INDEC code.
                               # "keep": separate markets, covariates repeated,
                               #   which double counts the population.
STATION_AGG    <- "simple"     # "simple": unweighted mean over the outlets.
                               # "volume": weighted by each outlet's volume,
                               #   which ties the characteristic to quantity.

FILE_IN  <- fs::path(DIR_INT, paste0("demanda_blp_sample_", PRODUCT_SYSTEM, ".csv"))
FILE_OUT <- fs::path(DIR_INT, paste0("estimation_sample_", PRODUCT_SYSTEM, ".csv"))
FILE_COV <- fs::path(DIR_INT, paste0("estimation_sample_", PRODUCT_SYSTEM, "_cobertura.csv"))

d <- fread(FILE_IN, encoding = "UTF-8")
n0 <- nrow(d)
cat("demand sample:", n0, "rows,", uniqueN(d$market_ids), "markets\n")

# A join must not change the number of rows. Anything else is a fan-out or a
# drop, and both are silent unless they are checked.
check_rows <- function(dt, step) {
  if (nrow(dt) != n0) stop(step, ": rows went from ", n0, " to ", nrow(dt))
  cat(sprintf("  %-46s rows unchanged\n", step))
  invisible(dt)
}

# 1. The bridge to the INDEC code ----

xw <- fread(fs::path(DIR_COV, "crosswalk_depto_codigo.csv"), encoding = "UTF-8")
xw <- unique(xw[, .(provincia, departamento, codigo_departamento_indec)])

d <- merge(d, xw, by = c("provincia", "departamento"), all.x = TRUE, sort = FALSE)
check_rows(d, "bridge to the INDEC code")

sin_codigo <- d[is.na(codigo_departamento_indec)]
cat("  rows with no INDEC code:", nrow(sin_codigo),
    "in", uniqueN(sin_codigo$market_ids), "markets\n")
if (nrow(sin_codigo) > 0) {
  cat("  they are the placeholder department and are dropped:\n")
  print(unique(sin_codigo[, .(provincia, departamento)]))
  d <- d[!is.na(codigo_departamento_indec)]
  n0 <- nrow(d)
}

# Departments that share a code with another department
compartidos <- xw[, .N, by = codigo_departamento_indec][N > 1, codigo_departamento_indec]
afectados <- d[codigo_departamento_indec %in% compartidos]
cat("  markets on a shared INDEC code:", uniqueN(afectados$market_ids), "\n")

if (SHARED_CODE == "collapse" && nrow(afectados) > 0) {
  # One market per code and quarter: quantities add, prices are weighted by
  # quantity, and the market size is taken once instead of twice.
  d[, mercado_key := fifelse(codigo_departamento_indec %in% compartidos,
                             paste(codigo_departamento_indec, trimestre, sep = "|"),
                             market_ids)]
  d <- d[, .(
    prices       = sum(prices * quantity) / sum(quantity),
    quantity     = sum(quantity),
    market_size  = market_size[1],
    outlets      = sum(outlets),
    shares       = sum(quantity) / market_size[1],
    n_productos_mercado = .N,
    n_otras_marcas      = uniqueN(firm_ids) - 1,
    provincia = provincia[1], departamento = paste(sort(unique(departamento)), collapse = " + "),
    trimestre = trimestre[1], anio = anio[1],
    codigo_departamento_indec = codigo_departamento_indec[1]
  ), by = .(market_ids = mercado_key, product_ids, firm_ids, grade)]
  d[, outside_share := 1 - sum(shares), by = market_ids]
  n0 <- nrow(d)
  cat("  collapsed to", uniqueN(d$market_ids), "markets,", n0, "rows\n")
}

# 2. Market covariates, on the INDEC code ----

d[, anio_int := as.integer(anio)]

pob <- fread(fs::path(DIR_COV, "covar_poblacion_depto.csv"), encoding = "UTF-8")
pob <- unique(pob[, .(codigo_departamento_indec, anio_int = as.integer(anio), poblacion)])
d <- merge(d, pob, by = c("codigo_departamento_indec", "anio_int"), all.x = TRUE, sort = FALSE)
check_rows(d, "population by department and year")

ing <- fread(fs::path(DIR_COV, "covar_ingreso_empleo_depto_ext.csv"), encoding = "UTF-8")
ing[, trimestre := paste0(anio, "T", ceiling(as.integer(mes) / 3))]
# Monthly wages and registered jobs, averaged to the quarter of the market.
COLS_ING <- c("w_mean_total", "w_mean_priv", "puestos_total", "puestos_priv")
ing_q <- ing[, lapply(.SD, mean, na.rm = TRUE),
             by = .(codigo_departamento_indec, trimestre), .SDcols = COLS_ING]
d <- merge(d, ing_q, by = c("codigo_departamento_indec", "trimestre"), all.x = TRUE, sort = FALSE)
check_rows(d, "wages by department and quarter")

# 3. Station variables, averaged over the outlets of each product-market ----

# The link from outlets to product-markets has to come from the panel: it is
# what says which outlet sold which product in which department and quarter.
BASE <- fs::path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
PRODS <- list(nafta  = c("Nafta (súper) entre 92 y 95 Ron", "Nafta (premium) de más de 95 Ron"),
              gasoil = c("Gas Oil Grado 2", "Gas Oil Grado 3"))[[PRODUCT_SYSTEM]]

b <- readRDS(BASE); setDT(b)
b <- b[canal_de_comercializacion == "Al público" & producto %in% PRODS]
b[, v := as.numeric(volumen)]
b <- b[!is.na(v) & v > 0]
b[, trimestre := paste0(anio, "T", ceiling(as.integer(mes) / 3))]
b[, marca := fifelse(is.na(bandera) | trimws(bandera) == "", "BLANCA", as.character(bandera))]
link <- b[, .(peso = sum(v)), by = .(provincia, departamento, trimestre,
                                     product_ids = paste(marca, producto, sep = "|"),
                                     nro_inscripcion, anio_int = as.integer(anio))]
rm(b); invisible(gc())

esp <- fread(fs::path(DIR_INT, "variables_espaciales_estacion.csv"), encoding = "UTF-8")
link <- merge(link, esp[, .(nro_inscripcion, d_refineria_km)], by = "nro_inscripcion", all.x = TRUE)

pan <- fs::path(DIR_INT, "variables_espaciales_panel.csv")
if (fs::file_exists(pan)) {
  vp <- fread(pan, encoding = "UTF-8")
  cols <- intersect(c("d_rival_min", "d_rival_otra_marca", "n_riv_5km", "cobertura_exacta"), names(vp))
  link <- merge(link, unique(vp[, c("nro_inscripcion", "anio", ..cols)]),
                by.x = c("nro_inscripcion", "anio_int"), by.y = c("nro_inscripcion", "anio"),
                all.x = TRUE)
}

vars_est <- setdiff(names(link), c("nro_inscripcion", "provincia", "departamento",
                                   "trimestre", "product_ids", "peso", "anio_int"))
agg_one <- function(x, w) {
  ok <- !is.na(x)
  if (!any(ok)) return(NA_real_)
  if (STATION_AGG == "volume") sum(x[ok] * w[ok]) / sum(w[ok]) else mean(x[ok])
}
est <- link[, lapply(.SD, agg_one, w = peso), .SDcols = vars_est,
            by = .(provincia, departamento, trimestre, product_ids)]

# After collapsing, the department label of a shared code is "A + B"; match on
# the components instead.
if (SHARED_CODE == "collapse") {
  est <- merge(est, unique(xw[, .(provincia, departamento, codigo_departamento_indec)]),
               by = c("provincia", "departamento"), all.x = TRUE)
  est <- est[, lapply(.SD, mean, na.rm = TRUE), .SDcols = vars_est,
             by = .(codigo_departamento_indec, trimestre, product_ids)]
  d <- merge(d, est, by = c("codigo_departamento_indec", "trimestre", "product_ids"),
             all.x = TRUE, sort = FALSE)
} else {
  d <- merge(d, est, by = c("provincia", "departamento", "trimestre", "product_ids"),
             all.x = TRUE, sort = FALSE)
}
check_rows(d, "station variables, averaged over the product-market")

# 4. Coverage ----

# What share of the rows has each variable, overall and by period. This is what
# says which variables can carry which years: the refinery runs start in 2010,
# and the wage series does not reach the first years either.
cobertura <- rbindlist(lapply(setdiff(names(d), c("market_ids", "product_ids")), function(v) {
  x <- d[[v]]
  data.table(variable = v,
             cobertura_total = round(100 * mean(!is.na(x)), 1),
             cobertura_2004_2009 = round(100 * mean(!is.na(x[d$anio_int <= 2009])), 1),
             cobertura_2010_2024 = round(100 * mean(!is.na(x[d$anio_int >= 2010])), 1))
}))
setorder(cobertura, cobertura_total)
print(cobertura)
fwrite(cobertura, FILE_COV, bom = TRUE)

fwrite(d, FILE_OUT, bom = TRUE)
cat("\nsaved", basename(FILE_OUT), "|", nrow(d), "rows,", uniqueN(d$market_ids), "markets\n")
cat("coverage table in", basename(FILE_COV), "\n")

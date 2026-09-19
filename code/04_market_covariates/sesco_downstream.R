# sesco_downstream.R
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

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})
source("code/00_config.R")

DIR  <- DIR_COVAR
CACHE <- fs::path(DIR, "raw_sesco")   # raw files, kept inside the data tree
stopifnot(dir.exists(CACHE))   # if missing, download the files again from the CKAN dataset above

# 1. Sales to the market (sales within the sector excluded), 6.5 M rows ----
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

# 2. Refining: inputs processed and by-products, by refinery and month ----
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

# 3. Foreign trade: quantity and value -> unit value (observed import parity) ----
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

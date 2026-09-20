# 11_demand_sample.R
# Assembles the product-market file the demand model is estimated on, and the
# instruments that go with it. The estimation itself runs in PyBLP; this script
# produces its input and nothing else.
#
# Input:  the analysis panel with the department column, the market covariates
#         written by 09_market_data.R, and the station variables of
#         08_station_variables.R
# Output: demanda_blp_sample.csv in DIR_INTERIM, one row per market and product
#
# A market is a department in a quarter. A product is a bandera (brand) crossed
# with a grade, with the unbranded outlets pooled into one composite brand.
# Gasoline and diesel are separate systems and the argument of the script picks
# one; gasoline is the one the main result rests on.
#
# Three choices this script does not make on its own, because they are open and
# are argued in docs/open_issues.md:
#   - the outlets that report volume in litres are rescaled here only if
#     RESCALE_LITRES is TRUE, which is off until the rule is settled (issue 1);
#   - market size has three candidate definitions and MARKET_SIZE picks one;
#   - the price is the one reported net of taxes, with the reconstruction from
#     the pump price and the tax schedule still to be built (issue 3).
# Running it with different settings is the point: the estimation should be
# repeated across them rather than reported for one.

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})

source("code/00_config.R")

DIR_INT <- DIR_INTERIM
DIR_COV <- DIR_COVAR

# Settings ----

PRODUCT_SYSTEM <- "nafta"   # "nafta" or "gasoil": the two are estimated apart
MARKET_SIZE    <- "max_adyacente"  # see market_size() below
RESCALE_LITRES <- FALSE     # issue 1: off until the rule is settled

PRODS <- list(
  nafta  = c("Nafta (súper) entre 92 y 95 Ron", "Nafta (premium) de más de 95 Ron"),
  gasoil = c("Gas Oil Grado 2", "Gas Oil Grado 3")
)[[PRODUCT_SYSTEM]]

BASE <- fs::path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")

# 1. Outlet-month panel, restricted to the retail channel ----

b <- readRDS(BASE)
setDT(b)
b <- b[canal_de_comercializacion == "Al público" & producto %in% PRODS]
b[, `:=`(v = as.numeric(volumen), p = as.numeric(precio_sin_impuestos))]
b <- b[!is.na(v) & v > 0 & !is.na(p) & p > 0.1]

# The market of the demand model. Quarters, not months: the set of outlets in a
# department moves slowly and a monthly market multiplies the cells without
# adding variation.
b[, trimestre := paste0(anio, "T", ceiling(as.integer(mes) / 3))]
b[, mercado := paste(provincia, departamento, trimestre, sep = "|")]

# The composite brand. Unbranded outlets are one product, not one per outlet:
# they carry no brand a consumer chooses between.
b[, marca := fifelse(is.na(bandera) | trimws(bandera) == "", "BLANCA", as.character(bandera))]
b[, producto_id := paste(marca, producto, sep = "|")]

cat("outlet-months:", nrow(b), "| markets:", uniqueN(b$mercado),
    "| products:", uniqueN(b$producto_id), "\n")

# 2. Product-market aggregates ----

# Price is the median across the outlets of the product in the market, so that
# one outlet with a stale report does not move the cell. Quantity is the sum.
pm <- b[, .(
  q       = sum(v),
  price   = median(p),
  bocas   = uniqueN(nro_inscripcion),
  anio    = anio[1],
  provincia = provincia[1],
  departamento = departamento[1]
), by = .(mercado, trimestre, producto_id, marca, producto)]

# 3. Market size and shares ----

# Market size must not contain the quantity of the quarter it scales, or the
# share is mechanically related to its own denominator. The three candidates
# differ in how much of the outside option they allow.
market_size <- function(pm, rule) {
  tot <- pm[, .(q_mercado = sum(q)), by = .(provincia, departamento, trimestre, anio)]
  setorder(tot, provincia, departamento, trimestre)
  switch(rule,
    # 1.5 times the largest quarter of the adjacent years, the definition the
    # roadmap starts from
    max_adyacente = tot[, M := 1.5 * max(q_mercado), by = .(provincia, departamento, anio)],
    # the same, using only years already observed, so nothing looks ahead
    max_pasado    = tot[, M := 1.5 * cummax(q_mercado), by = .(provincia, departamento)],
    # a physical potential from population and car ownership, which needs the
    # covariates of 09_market_data.R and is not built here yet
    potencial     = stop("the physical market size is not built yet (see docs/open_issues.md)"),
    stop("unknown market size rule: ", rule)
  )
  tot[, .(provincia, departamento, trimestre, M)]
}

M <- market_size(pm, MARKET_SIZE)
pm <- merge(pm, M, by = c("provincia", "departamento", "trimestre"))

pm[, share := q / M]
pm[, share_total := sum(share), by = mercado]
pm[, share_0 := 1 - share_total]

cat("cells with a non-positive outside share:",
    pm[share_0 <= 0, uniqueN(mercado)], "of", uniqueN(pm$mercado), "markets\n")
cat("the analysis plan asks for fewer than 2% of cells with an outside share below 0.05\n")

# On the sample as it stands the criterion is met everywhere, but it is met by
# construction rather than by the data. With M = 1.5 times the largest quarter,
# a market in its own largest quarter has an outside share of exactly 1/3, and
# no market can fall below that. The criterion was written to rule out the
# earlier definition, which left a quarter of the cells with no outside good at
# all; it does not discriminate among the candidates here. What separates them
# is what they imply about the elasticity, which only the estimation shows.

# 4. Instruments ----

# The price instrument is a supply shock that moves the cost of serving a market
# without moving the taste for its products: deviations of refinery throughput
# from its normal level, carried to each department by inverse distance to the
# refineries that supply it. Refinery runs start in 2010, which is why the
# instrument is not available for the first years of the panel.
#
# Counts of rival products identify the random coefficients, in the usual BLP
# way: what a market offers besides a product shifts the substitution towards it
# without shifting its own quality.
#
# Prices in other markets are not used. YPF prices close to nationally, so a
# price elsewhere carries the same brand-level shock as the price here, which is
# the objection Bresnahan raised to that instrument.

# 4.1 Rival counts, which need nothing outside this file
pm[, n_productos_mercado := .N, by = mercado]
pm[, n_otras_marcas := uniqueN(marca) - 1, by = mercado]
pm[, n_mismo_grado := .N - 1, by = .(mercado, producto)]

# 4.2 The refinery shock, from the distance to each refinery and the runs
# reported by the Energy Secretariat. Both pieces exist: d_refineria_km in
# variables_espaciales_estacion.csv, and covar_sesco_refinacion.csv. Writing the
# weights is the next step and is deliberately left here rather than improvised.
FILE_REF <- fs::path(DIR_INT, "variables_espaciales_estacion.csv")
FILE_RUN <- fs::path(DIR_COV, "covar_sesco_refinacion.csv")
if (fs::file_exists(FILE_REF) && fs::file_exists(FILE_RUN)) {
  cat("refinery distance and refinery runs are both present; the shock is built in the next commit\n")
} else {
  cat("refinery distance or refinery runs missing: the shock cannot be built here\n")
}

# 5. Save ----

out <- pm[, .(market_ids = mercado, product_ids = producto_id,
              firm_ids = marca, grade = producto,
              prices = price, shares = share, outside_share = share_0,
              quantity = q, market_size = M, outlets = bocas,
              n_productos_mercado, n_otras_marcas, n_mismo_grado,
              provincia, departamento, trimestre, anio)]

FILE_OUT <- fs::path(DIR_INT, paste0("demanda_blp_sample_", PRODUCT_SYSTEM, ".csv"))
fwrite(out, FILE_OUT, bom = TRUE)
cat("saved", basename(FILE_OUT), "|", nrow(out), "rows,", uniqueN(out$market_ids), "markets\n")

# population_census_anchored.R
# Variant of the department population series anchored to the 2022 census.
#
# Input:  covar_poblacion_depto.csv (from population.R, not modified),
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

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})
source("code/00_config.R")

DIR  <- DIR_COVAR

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

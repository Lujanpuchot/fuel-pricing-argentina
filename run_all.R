# run_all.R
# Runs the pipeline in order, from the repository root:
#
#   Rscript run_all.R
#
# Each script reads what the previous one wrote, so they can also be run one at a
# time, and each of them can be restarted at any of its parts. Set FUEL_DATA_ROOT
# first (see code/00_config.R).
#
# 02_geocode_stations.R queries public services one address at a time and takes
# hours on a first run. It keeps what it has already resolved, so interrupting it
# and running it again is safe.

scripts <- c(
  "code/01_clean_panel.R",       # raw files -> analysis panel, with markets
  "code/02_geocode_stations.R",  # coordinates for every outlet
  "code/03_station_variables.R", # rivals, distances, highway and border indicators
  "code/04_market_data.R",       # population, wages, prices, costs by market
  "code/05_descriptives.R"       # tables and figures
)

for (s in scripts) {
  message("\n---- ", s)
  started <- Sys.time()
  source(s, echo = FALSE)
  message(sprintf("---- %s finished in %.1f minutes", s,
                  as.numeric(difftime(Sys.time(), started, units = "mins"))))
}

# code/diagnostics_data_quality.R is not part of the pipeline. It documents how
# the cleaning thresholds were chosen and does not modify the panel.

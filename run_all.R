# run_all.R
# Runs the whole pipeline from the repository root:
#
#   Rscript run_all.R
#
# The scripts can also be run one at a time, in this order. Each one reads the
# file the previous one wrote, so the pipeline can be picked up where it stopped.
# Paths are set in code/00_config.R.
#
# 07_geocode_stations.R takes hours on a first run, because it queries public
# services one address at a time. It saves what it resolves as it goes, so
# interrupting it and running it again later is safe.

source("code/01_build_panel.R")      # the six raw files -> one panel
source("code/02_clean_volume.R")     # parse volume, drop implausible values
source("code/03_clean_prices.R")     # drop corrupt prices
source("code/04_deduplicate.R")      # remove duplicate records
source("code/05_business_type.R")    # infer the type of each outlet
source("code/06_markets.R")          # locality -> department, the market of the model

source("code/07_geocode_stations.R")  # coordinates for every outlet
source("code/08_station_variables.R") # rivals, distances, highway and border flags
source("code/09_market_data.R")       # population, wages, prices and costs by market
source("code/10_descriptives.R")      # tables and figures

source("code/11_demand_sample.R")     # the product-market file the demand model needs
source("code/12_estimation_sample.R") # market and station variables joined onto it

# code/diagnostics_data_quality.R is not part of the pipeline: it documents how
# the cleaning thresholds were chosen and does not modify the panel.
#
# code/13_estimation.py is the demand estimation and runs in Python, on the file
# 12_estimation_sample.R writes.

# 00_config.R
# Project paths. Every script sources this file first and is meant to be run
# from the repository root.
#
# Raw and intermediate data live outside the repository (about 10 GB). Point the
# environment variable FUEL_DATA_ROOT to that folder, for instance in ~/.Renviron:
#   FUEL_DATA_ROOT=D:/fuel-thesis

ROOT <- Sys.getenv("FUEL_DATA_ROOT")
if (!nzchar(ROOT) || !dir.exists(ROOT)) {
  stop("FUEL_DATA_ROOT is not set or does not exist (see 'Running the code' in the README).")
}
ROOT <- normalizePath(ROOT, winslash = "/")

# Folder names follow the layout of the data tree on disk.
DIR_RETAIL    <- file.path(ROOT, "Datos", "Principal", "Minoristas", "Última versión")  # raw station-level files
DIR_INTERIM   <- file.path(DIR_RETAIL, "Intermedio")             # cleaned panels and station-level files
DIR_COVAR     <- file.path(DIR_INTERIM, "covariables_mercado")   # market-level covariates
DIR_WHOLESALE <- file.path(ROOT, "Datos", "Principal", "Mayoristas")
DIR_OUTPUT    <- file.path(ROOT, "Documentos", "2. Gráficos y tablas descriptivas - new")

# Contact address sent in the User-Agent of geocoding requests, as the Nominatim
# usage policy asks for.
CONTACT_EMAIL <- Sys.getenv("FUEL_CONTACT_EMAIL", "name@example.org")

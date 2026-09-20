# 00_config.R
# Paths. Every script starts by sourcing this file and is meant to be run from
# the repository root.
#
# The data are not in the repository: the raw and intermediate files add up to
# about 10 GB. ROOT is the folder that holds them, with the tree described in
# the README. It is looked up in this order:
#
#   1. the environment variable FUEL_DATA_ROOT, if it is set;
#   2. Dropbox/Trabajo/Tesis under the user's home folder, which is where I keep
#      the project;
#   3. any other path added to .candidates below.
#
# So nothing has to be configured on my machine, and on any other one it is
# enough to set the variable, for instance in ~/.Renviron:
#
#   FUEL_DATA_ROOT=D:/tesis
#   FUEL_CONTACT_EMAIL=name@example.org

.candidates <- c(
  Sys.getenv("FUEL_DATA_ROOT"),
  file.path(Sys.getenv("USERPROFILE"), "Dropbox", "Trabajo", "Tesis"),
  file.path(Sys.getenv("HOME"), "Dropbox", "Trabajo", "Tesis")
)

ROOT <- .candidates[nzchar(.candidates) & dir.exists(.candidates)][1]

if (is.na(ROOT)) {
  stop("Data folder not found. Set FUEL_DATA_ROOT to the folder holding the data ",
       "tree, or add its path to .candidates in code/00_config.R. Tried:\n  ",
       paste(.candidates[nzchar(.candidates)], collapse = "\n  "))
}

ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
message("ROOT = ", ROOT)

# Folders inside ROOT. The names are the ones the folders have on disk.
DIR_RETAIL    <- file.path(ROOT, "Datos", "Principal", "Minoristas", "Última versión")
DIR_INTERIM   <- file.path(DIR_RETAIL, "Intermedio")
DIR_COVAR     <- file.path(DIR_INTERIM, "covariables_mercado")
DIR_WHOLESALE <- file.path(ROOT, "Datos", "Principal", "Mayoristas")
DIR_OUTPUT    <- file.path(ROOT, "Documentos", "2. Gráficos y tablas descriptivas - new")

# Sent in the User-Agent of the geocoding requests, as the Nominatim usage
# policy asks. Set FUEL_CONTACT_EMAIL before running 07_geocode_stations.R.
CONTACT_EMAIL <- Sys.getenv("FUEL_CONTACT_EMAIL", "name@example.org")

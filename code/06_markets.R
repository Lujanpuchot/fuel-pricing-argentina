# 06_markets.R
# Build (or load and validate) the locality -> department crosswalk and apply it
# to the station panel, which leaves the final panel with a `departamento` column.
#
# Input:  eess_all_cleaned7_alternative_sinceappearance.rds
#         crosswalk_localidad_departamento.csv  (frozen crosswalk, read by default)
#         crosswalk_correcciones_auditoria.csv  (reviewed corrections; rebuild only)
#         georef_departamentos_ref.csv          (official list of departments)
# Output: eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds
#         crosswalk_localidad_departamento.csv  (rewritten only when rebuilding)
#
# Why departments rather than localities. A market in the demand model is a
# province x department x month. The locality is too fine: almost half of the
# localities are served by a single brand, which leaves substitution
# unidentified. The department pools neighboring localities, removes these
# spurious monopolies and keeps the periphery in the sample. In 2024 (channel
# "Al público", one bandera (brand) per station, unbranded stations counted as
# individual firms):
#   stations in a monopoly market:  11.45% by locality -> 2.02% by department
#                                   (core 9.83 -> 0.35, periphery 15.17 -> 5.84)
#   monopoly markets:               43.87% by locality -> 17.91% by department
#
# The crosswalk covers 1,398 (province, locality) pairs. Each pair was checked
# against georef by province id, the residual was assigned from the station
# addresses and external sources, and the resulting 60 corrections are listed,
# with the reason for each one, in crosswalk_correcciones_auditoria.csv
# (crosswalk_revisados_auditoria.csv holds the cases reviewed and left unchanged).
#
# crosswalk_localidad_departamento.csv, with a `fuente` column that records where
# each assignment comes from, is kept frozen as reviewed reference data: by
# default the script reads it, validates it and applies it. Setting
# REBUILD_FROM_API to TRUE rebuilds it from the georef API (needs internet
# access) with the same corrections and checks. georef can change between runs,
# so if the two versions differ the frozen file prevails.
#
# 95 rows of the panel (locality "N/D", a single station) are left with a missing
# department on purpose: "N/D" is a placeholder for missing data. Exclude them
# from the model.

suppressPackageStartupMessages({library(data.table); library(httr); library(jsonlite)})
source("code/00_config.R")

REBUILD_FROM_API <- FALSE   # TRUE rebuilds the crosswalk from georef (about 5 minutes)

# Paths ----
DIR_INT  <- DIR_INTERIM
BASE_IN  <- file.path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance.rds")
XW_OUT   <- file.path(DIR_INT, "crosswalk_localidad_departamento.csv")
BASE_OUT <- file.path(DIR_INT, "eess_all_cleaned7_alternative_sinceappearance_con_crosswalk.rds")
XW_CORR  <- file.path(DIR_INT, "crosswalk_correcciones_auditoria.csv")   # sep = "|"
REF_DEP  <- file.path(DIR_INT, "georef_departamentos_ref.csv")           # official list of departments

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Normalization helpers ----
strip_acc <- function(x) chartr("áéíóúüñÁÉÍÓÚÜÑàèìòù", "aeiouunAEIOUUNaeiou", x)
nk        <- function(x){ x <- toupper(strip_acc(trimws(x))); gsub("\\s+", " ", x) }

# Expand the abbreviations found in locality names before querying georef
normx <- function(s){
  s <- toupper(trimws(s))
  reps <- c("CNEL\\."="CORONEL","GRAL\\."="GENERAL","ING\\."="INGENIERO","VTE\\."="VICENTE",
            "GDOR\\."="GOBERNADOR","GOB\\."="GOBERNADOR","CAP\\."="CAPITAN","PTE\\."="PRESIDENTE",
            "PCIA\\. DE LA PLAZA"="PRESIDENCIA DE LA PLAZA","PCIA\\."="PRESIDENCIA",
            "DR\\."="DOCTOR","ALTE\\."="ALMIRANTE","SGO\\."="SANTIAGO","PTO\\."="PUERTO",
            "S\\.F\\.V\\. DE"="SAN FERNANDO DEL VALLE DE","S\\.A\\. DE"="SAN ANTONIO DE",
            "S\\.M\\. DE"="SAN MIGUEL DE","L\\.N\\."="LEANDRO N","FCO\\.?"="FRANCISCO",
            "J\\. ?B\\."="JUAN B","R\\. DE ESCALADA"="REMEDIOS DE ESCALADA",
            "R\\. SOURDEAUX"="INGENIERO ADOLFO SOURDEAUX","H\\. ASCASUBI"="HILARIO ASCASUBI",
            "G\\. LAFERRERE"="GREGORIO DE LAFERRERE","GREGORIO DE LA FERRERE"="GREGORIO DE LAFERRERE",
            "EL TALAR DE PACHECO"="EL TALAR","VEINTICINCO DE MAYO"="25 DE MAYO",
            "3 DE FEBRERO"="CASEROS")
  for (k in names(reps)) s <- gsub(k, reps[[k]], s)
  s <- gsub("O' ", "O'", s); gsub("\\s+", " ", trimws(s))
}

# georef is queried by province id. Filtering by province name is ambiguous:
# "BUENOS AIRES" also matches "Ciudad Autónoma de Buenos Aires", which sends
# San Nicolás (province of Buenos Aires) to Comuna 1 of the city.
PID <- c("BUENOS AIRES"="06","CATAMARCA"="10","CHACO"="22","CHUBUT"="26","CORDOBA"="14",
         "CORRIENTES"="18","ENTRE RIOS"="30","FORMOSA"="34","JUJUY"="38","LA PAMPA"="42",
         "LA RIOJA"="46","MENDOZA"="50","MISIONES"="54","NEUQUEN"="58","RIO NEGRO"="62",
         "SALTA"="66","SAN JUAN"="70","SAN LUIS"="74","SANTA CRUZ"="78","SANTA FE"="82",
         "SANTIAGO DEL ESTERO"="86","TIERRA DEL FUEGO"="94","TUCUMAN"="90")

# Validation checks (stop the script if the crosswalk is inconsistent) ----
validate_xw <- function(xw, base_pairs){
  # 1) structure: one row per (province, locality); a missing department is
  #    accepted only under fuente "sin_dato"
  stopifnot(nrow(xw) == uniqueN(xw[, .(provincia, localidad)]))
  n_na <- xw[is.na(departamento) & fuente != "sin_dato", .N]
  if (n_na > 0) stop("Check failed: ", n_na, " departments are NA outside fuente='sin_dato'")
  # 2) coverage: every pair in the panel has a row in the crosswalk
  anti <- base_pairs[!unique(xw[, .(provincia, loc = trimws(localidad))]),
                     on = c("provincia","loc")]
  if (nrow(anti) > 0) { print(anti); stop("Check failed: pairs in the panel missing from the crosswalk") }
  # 3) a single label per department, so that spelling variants do not split a
  #    market (e.g. 'Fontana')
  lbl <- xw[!is.na(departamento),
            .(n = uniqueN(departamento)), by = .(provincia, dep_k = nk(departamento))][n > 1]
  if (nrow(lbl) > 0) { print(lbl); stop("Check failed: same department under different labels") }
  # 4) each department belongs to its province according to the official list
  #    (local file, no API call)
  if (file.exists(REF_DEP)) {
    ref <- fread(REF_DEP, encoding = "UTF-8")
    pm  <- c("CAPITAL FEDERAL"="CIUDAD AUTONOMA DE BUENOS AIRES",
             "TIERRA DEL FUEGO"="TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR")
    off <- unique(rbind(ref[, .(prov_k = nk(provincia), dep_k = nk(departamento))],
                        data.table(prov_k = "CIUDAD AUTONOMA DE BUENOS AIRES", dep_k = "CABA")))
    chk <- xw[!is.na(departamento) & !fuente %in% c("paraje","sin_dato")]
    chk[, prov_k := nk(provincia)][provincia %in% names(pm), prov_k := pm[provincia]]
    chk[, dep_k := nk(departamento)]
    bad <- chk[!off, on = c("prov_k","dep_k")]
    if (nrow(bad) > 0) { print(bad[, .(provincia, localidad, departamento, fuente)])
                         stop("Check failed: department outside its province") }
    message(sprintf("  checks passed: %d pairs validated against the official list (%d paraje, %d sin_dato exempt)",
                    nrow(chk), xw[fuente=="paraje",.N], xw[fuente=="sin_dato",.N]))
  } else {
    message("  partial checks: ", basename(REF_DEP), " not found (province membership check skipped)")
  }
  invisible(TRUE)
}

# Station panel ----
b <- readRDS(BASE_IN); setDT(b)
base_pairs <- unique(b[, .(provincia, loc = trimws(localidad))])

# 1. Get the crosswalk: read the frozen file, or rebuild it and correct it ----
if (!REBUILD_FROM_API && file.exists(XW_OUT)) {

  message("Loading frozen crosswalk: ", basename(XW_OUT))
  xw <- fread(XW_OUT, encoding = "UTF-8")
  if (!"fuente" %in% names(xw)) xw[, fuente := "georef"]   # file saved without the fuente column
  xw <- xw[, .(provincia, localidad, departamento = as.character(departamento), fuente)]
  xw[trimws(departamento) == "" | departamento == "NA", departamento := NA]

} else {

  message("Rebuilding crosswalk from the georef API (one GET per pair, by province id) ...")
  pairs <- unique(b[, .(provincia, localidad = trimws(localidad))])[order(provincia, localidad)]
  message(sprintf("  province-locality pairs: %d", nrow(pairs)))

  # GET with up to 3 attempts and increasing waits. A municipality is not a
  # department, so /municipios is never queried. The department is read from
  # departamento_nombre, except in /departamentos, where it is nombre. With
  # max = 1 georef returns its best fuzzy match, which can be another locality
  # (La Niña -> Arrecifes); those cases are handled in the corrections file.
  GET_dep <- function(path, nombre, provid){
    for (a in 1:3){
      resp <- tryCatch(GET(sprintf("https://apis.datos.gob.ar/georef/api/%s", path),
                           query = list(nombre = nombre, provincia = provid,
                                        max = 1, aplanar = "true"),
                           timeout(60)), error = function(e) NULL)
      if (!is.null(resp) && status_code(resp) == 200){
        r <- fromJSON(content(resp, "text", encoding = "UTF-8"))[[gsub("-","_",path)]]
        if (is.null(r) || length(r) == 0) return(NA_character_)
        col <- if (path == "departamentos") "nombre" else "departamento_nombre"
        if (!col %in% names(as.data.table(r))) return(NA_character_)
        return(as.character(as.data.table(r)[[col]][1]))
      }
      Sys.sleep(1.5 * a)
    }
    stop("no response from georef after 3 attempts: ", path, " / ", nombre)  # fail rather than return a silent NA
  }

  pairs[, `:=`(departamento = NA_character_, fuente = NA_character_)]
  pairs[provincia == "CAPITAL FEDERAL", `:=`(departamento = "CABA", fuente = "caba")]

  cascada <- c("localidades-censales","localidades","asentamientos","departamentos")
  for (i in which(is.na(pairs$departamento))) {
    provid <- PID[[pairs$provincia[i]]]
    for (path in cascada) {
      for (nom in unique(c(normx(pairs$localidad[i]), pairs$localidad[i]))) {
        got <- GET_dep(path, nom, provid)
        if (!is.na(got)) { pairs[i, `:=`(departamento = got,
                                         fuente = paste0("georef_", path))]; break }
      }
      if (!is.na(pairs$departamento[i])) break
    }
    if (i %% 100 == 0) message(sprintf("  ... %d/%d", i, nrow(pairs)))
  }
  message(sprintf("  unmatched after georef: %d", pairs[is.na(departamento), .N]))

  # Manual dictionary for the residual (mostly Greater Buenos Aires and Córdoba).
  # 'Mayor Luis J. Fontana' is the official spelling of that department.
  manual <- fread(sep = "|", text =
'provincia|localidad|departamento
BUENOS AIRES|ING. MASCHWITZ|Escobar
BUENOS AIRES|GREGORIO DE LA FERRERE|La Matanza
BUENOS AIRES|G. LAFERRERE|La Matanza
BUENOS AIRES|VILLA CELINA|La Matanza
BUENOS AIRES|VILLA INSUPERABLE|La Matanza
BUENOS AIRES|SAN FCO.SOLANO|Quilmes
BUENOS AIRES|SAN FCO. SOLANO|Quilmes
BUENOS AIRES|R. DE ESCALADA|Lanús
BUENOS AIRES|ING. WHITE|Bahía Blanca
BUENOS AIRES|GRAL. VIAMONTE|General Viamonte
BUENOS AIRES|VILLARINO|Villarino
BUENOS AIRES|BONIFACIO|Guaminí
BUENOS AIRES|VILLA MAZA|Adolfo Alsina
BUENOS AIRES|INGENIERO BUDGE|Lomas de Zamora
BUENOS AIRES|ISLA SAN FERNANDO|San Fernando
BUENOS AIRES|BANCALARI|Tigre
BUENOS AIRES|VILLA ALBERTINA|Lomas de Zamora
BUENOS AIRES|VILLA MOQUEHUA|Chivilcoy
BUENOS AIRES|AGUSTIN FERRARI|Merlo
BUENOS AIRES|PARAJE LA BALLENERA|General Pueyrredón
CORDOBA|MONTE CRISTO|Río Primero
CORDOBA|ARGUELLO|Capital
CORDOBA|W. ESCALANTE|Unión
CORDOBA|DALMACIO VELEZ SARSFIELD|Tercero Arriba
CORDOBA|VILLA ANIZACATE|Santa María
CORDOBA|VILLA MARIA DEL RIO SECO|Río Seco
CATAMARCA|VALLE VIEJO|Valle Viejo
CATAMARCA|SAN ANTONIO DE LA PAZ|El Alto
CHACO|CNEL. DUGRATY|Mayor Luis J. Fontana
CHACO|PCIA. DE LA PLAZA|Presidencia de la Plaza
CHACO|AVIATERAI|Independencia
CHUBUT|COLONIA SARMIENTO|Sarmiento
CHUBUT|CERRO DRAGON|Escalante
ENTRE RIOS|COLONIA YERUA|Concordia')
  pairs[manual, on = c("provincia","localidad"),
        `:=`(departamento = i.departamento, fuente = "manual")]

  # Reviewed corrections are applied last so that they override every other
  # source; the reason for each one is given in the file itself.
  if (!file.exists(XW_CORR)) stop("missing ", basename(XW_CORR), ": do not rebuild without the corrections")
  corr <- fread(XW_CORR, sep = "|", encoding = "UTF-8")
  pairs[corr, on = c("provincia","localidad"),
        `:=`(departamento = i.dep_nuevo, fuente = "correccion")]
  pairs[departamento == "__NA__", `:=`(departamento = NA_character_, fuente = "sin_dato")]

  # An isolated paraje (hamlet) that remains unresolved is its own market
  # (recorded as fuente 'paraje')
  pairs[is.na(departamento) & fuente != "sin_dato" | is.na(fuente),
        `:=`(departamento = localidad, fuente = "paraje")]

  xw <- pairs[, .(provincia, localidad, departamento, fuente)]
  message(sprintf("  coverage: %d pairs | %d province-department markets | sources: %s",
                  nrow(xw), uniqueN(xw[!is.na(departamento), .(provincia, departamento)]),
                  paste(xw[, .N, by=fuente][order(-N)][, sprintf("%s=%d", fuente, N)], collapse=", ")))
  validate_xw(xw, base_pairs)
  fwrite(xw, XW_OUT, bom = TRUE)
  message("  crosswalk saved: ", basename(XW_OUT))
}

message("Validating crosswalk ...")
validate_xw(xw, base_pairs)

# 2. Apply the crosswalk to the panel and save ----
n0 <- nrow(b)
# Merge on the trimmed locality on both sides (some localities in the panel have
# trailing spaces). xwk has a unique key so that the merge cannot duplicate rows.
xwk <- unique(xw[, .(provincia, loc_key = trimws(localidad), departamento)],
              by = c("provincia","loc_key"))
b[, loc_key := trimws(localidad)]
b   <- merge(b, xwk, by = c("provincia","loc_key"), all.x = TRUE)
b[, loc_key := NULL]
stopifnot(nrow(b) == n0)
n_na <- b[is.na(departamento), .N]
message(sprintf("  rows without department: %d  (expected: 95 = locality 'N/D')", n_na))
if (n_na != 95) warning(sprintf("unexpected number of NA rows: %d (expected 95)", n_na))

saveRDS(b, BASE_OUT)
message(sprintf("\nFinal panel with crosswalk saved (%d rows, %d columns):\n  %s",
                nrow(b), ncol(b), BASE_OUT))
message(sprintf("  province x department markets: %d",
                uniqueN(b[!is.na(departamento), .(provincia, departamento)])))

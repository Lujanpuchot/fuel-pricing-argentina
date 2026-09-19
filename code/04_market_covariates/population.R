# population.R
# Population by department and year, the market-size covariate of the BLP
# demand model.
#
# Input:  indec_proyeccion_departamentos_10_25.pdf (downloaded if missing),
#         cep_diccionario_depto.csv (written by wages_employment.R)
# Output: covar_poblacion_depto.csv
#
# Source: INDEC, "Estimaciones de población por sexo, departamento y año
# calendario 2010-2025" (Serie Análisis Demográfico 38). It is only published as
# a PDF, so the tables are parsed from the text. The series starts in 2010;
# 2004-2009 is backcast with each department's own 2010-2015 growth rate. An
# alternative that is not implemented: the INDEC 2001-2015 series (2001 census
# base), rescaled to match in 2010.
#
# The projection lines up with both censuses: the national total is 40.79 M in
# 2010 (census 40.12 M) and 46.23 M in 2022 (census 46.04 M, +0.4%).
#
# Output columns: codigo_departamento_indec, id_provincia_indec, provincia,
# departamento, anio, poblacion, fuente. Years 2004-2025, about 512 departments.

suppressPackageStartupMessages({
  library(pdftools)
  library(data.table)
  library(fs)
})
source("code/00_config.R")
options(timeout = 120)

DIR  <- DIR_COVAR
dir.create(DIR, showWarnings=FALSE, recursive=TRUE)
PDF  <- fs::path(DIR, "indec_proyeccion_departamentos_10_25.pdf")
DICC <- fs::path(DIR, "cep_diccionario_depto.csv")   # written by wages_employment.R
URL  <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/proyeccion_departamentos_10_25.pdf"

# Upper case without accents, to match department names across sources
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

if (!fs::file_exists(PDF)) {
  cat("downloading the INDEC PDF...\n")
  download.file(URL, PDF, mode="wb", quiet=TRUE)
}

# 1. Parse the PDF ----
# The text is read line by line, keeping track of the current province (one
# table per province, "Cuadro 1.1" to "Cuadro 1.24"), sex block and year header
# (each table shows 2010-2017 and 2018-2025 in separate blocks). Only the "Ambos
# sexos" (both sexes) rows are kept. CABA enters as a single department, from
# its total row; elsewhere the totals and aggregates matched by SKIP are dropped.
t <- pdf_text(PDF)
provs <- c("CABA","Buenos Aires","Catamarca","Chaco","Chubut","Córdoba","Corrientes",
  "Entre Ríos","Formosa","Jujuy","La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
  "Río Negro","Salta","San Juan","San Luis","Santa Cruz","Santa Fe","Santiago del Estero",
  "Tierra del Fuego","Tucumán")
names(provs) <- paste0("1.", 1:24)
SKIP <- "Partido|Departamento|Comuna|Cuadro|sexos|^Total$|provincia de|Gran Buenos Aires|Interior|Veinticuatro|partidos del|Población|estimada"

rows <- list()
prov <- NA_character_
sexo <- NA_character_
years <- NULL
N <- 0L
for (pg in t) for (ln in strsplit(pg, "\n")[[1]]) {
  # Table number -> province
  cm <- regmatches(ln, regexpr("Cuadro 1\\.\\d+", ln))
  if (length(cm)) {
    cn <- sub("Cuadro ", "", cm)
    if (!is.na(provs[cn])) prov <- provs[[cn]]
  }
  if (grepl("Ambos sexos", ln)) sexo <- "A"
  else if (grepl("\\bVarones\\b", ln)) sexo <- "V"
  else if (grepl("\\bMujeres\\b", ln)) sexo <- "M"
  # Year header of the current block
  if (grepl("20\\d\\d\\s+20\\d\\d", ln)) {
    years <- as.integer(regmatches(ln, gregexpr("20\\d\\d", ln))[[1]])
    N <- length(years)
    next
  }
  if (is.na(sexo) || sexo!="A" || is.null(years) || N==0L || is.na(prov)) next
  # Data row: a name followed by exactly N figures, with "." as thousands separator
  rx <- sprintf("^\\s*(.*?)((?:\\s+\\d{1,3}(?:\\.\\d{3})*){%d})\\s*$", N)
  m <- regmatches(ln, regexec(rx, ln))[[1]]
  if (!length(m)) next
  nm <- trimws(m[2])
  keep_total <- (prov=="CABA" && nm=="Total")
  if (prov=="CABA" && !keep_total) next
  if (!keep_total && (nm=="" || grepl(SKIP, nm, ignore.case=TRUE))) next
  v <- as.integer(gsub("\\.", "", strsplit(trimws(m[3]), "\\s+")[[1]]))
  if (length(v)!=N || anyNA(v)) next
  rows[[length(rows)+1]] <- data.table(provincia=prov, departamento=if(keep_total)"CABA" else nm, year=years, pob=v)
}
P <- dcast(unique(rbindlist(rows), by=c("provincia","departamento","year")), provincia+departamento ~ year, value.var="pob")
ycols <- as.character(2010:2025)
stopifnot(all(rowSums(!is.na(P[, ..ycols]))==16))   # every department has all 16 years
cat("parsed:", nrow(P), "departments | national total 2010 =", format(sum(P[["2010"]])), "| 2022 =", format(sum(P[["2022"]])), "\n")

# 2. Long format and 2004-2009 backcast ----
# Each department is carried back from 2010 at its own 2010-2015 annual growth rate.
L <- melt(P, id.vars=c("provincia","departamento"), measure.vars=ycols, variable.name="anio", value.name="poblacion")
L[, anio := as.integer(as.character(anio))]
g <- P[, .(provincia, departamento, p10=get("2010"), cagr=(get("2015")/get("2010"))^(1/5)-1)]
cj <- CJ(idx=seq_len(nrow(g)), anio=2004:2009)               # sorted by idx, so cj$anio lines up with g[cj$idx]
bc <- g[cj$idx][, anio := cj$anio][
       , .(provincia, departamento, anio, poblacion=round(p10/(1+cagr)^(2010-anio)))]
stopifnot(nrow(bc)==6L*nrow(g),                              # six backcast years per department
          all(bc[, uniqueN(anio), by=.(provincia,departamento)]$V1==6L))
L <- rbind(L, bc)
setorder(L, provincia, departamento, anio)

# 3. Map to INDEC department codes ----
# Names are matched to the CEP dictionary. The 11 departments in `fix` are
# written differently in the PDF (mostly abbreviations) and are coded by hand.
dic <- fread(DICC, encoding="UTF-8")
dic[, `:=`(pk=norm(nombre_provincia_indec), dk=norm(nombre_departamento_indec))]
L[, `:=`(pk=norm(provincia), dk=norm(departamento))]
L <- merge(L, dic[, .(pk, dk, codigo_departamento_indec, id_provincia_indec)], by=c("pk","dk"), all.x=TRUE)
fix <- data.table(
  dk = norm(c("Chascomús","1º de Mayo","Coronel Felipe Varela","General Angel V. Peñaloza","General Juan F. Quiroga",
              "General Ocampo","La Caldera","La Capital","Juan F. Ibarra","Juan B. Alberdi","Río Grande")),
  pk = norm(c("Buenos Aires","Chaco","La Rioja","La Rioja","La Rioja","La Rioja","Salta","San Luis",
              "Santiago del Estero","Tucumán","Tierra del Fuego")),
  cod_fix = c(6217L,22126L,46028L,46056L,46070L,46084L,66077L,74056L,86098L,90042L,94007L))
fix[, idp_fix := as.integer(cod_fix %/% 1000)]
L <- merge(L, fix, by=c("pk","dk"), all.x=TRUE)
L[is.na(codigo_departamento_indec) & !is.na(cod_fix), `:=`(codigo_departamento_indec=cod_fix, id_provincia_indec=idp_fix)]
cat("mapped to an INDEC code:", uniqueN(L[!is.na(codigo_departamento_indec),.(provincia,departamento)]),
    "of", uniqueN(L[,.(provincia,departamento)]), "(Antártida has no code and no relevant population)\n")

out <- L[, .(codigo_departamento_indec, id_provincia_indec, provincia, departamento, anio, poblacion,
             fuente = ifelse(anio>=2010, "INDEC proy 2010-2025", "backcast 2004-2009"))]
setorder(out, provincia, departamento, anio)
fwrite(out, fs::path(DIR, "covar_poblacion_depto.csv"), bom=TRUE)
cat("[covar_poblacion_depto.csv]", nrow(out), "rows |", uniqueN(out[,.(provincia,departamento)]), "departments | 2004-2025\n")

# Every year should show 512 departments and a national total of 39-46 M
chk <- out[, .(deptos=.N, tot=sum(poblacion)), by=anio][order(anio)]
cat("\nnational total by year:\n")
print(chk[anio %in% c(2004,2007,2009,2010,2015,2022,2025)])

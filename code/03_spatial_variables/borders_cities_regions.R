# borders_cities_regions.R
# Time-invariant geographic variables by station: distance to the border and to
# the nearest road crossing, to large cities and to Buenos Aires, Patagonia flag.
#
# Input:  geocodificacion_final.csv (locatable stations: precision exacta or localidad)
# Output: variables_geograficas_estacion.csv
#           d_frontera_km    km to the nearest international border (edge of the
#                            five neighboring countries)
#           d_cruce_km       km to the nearest international road crossing
#           cruce_cercano,
#           pais_cruce       name and country of that crossing
#           d_ciudad_km      km to the nearest large city (roughly above 100,000
#                            inhabitants)
#           ciudad_cercana   name of that city
#           d_gba_km         km to the city of Buenos Aires, the dominant market
#           patagonia_icl    1 if the province is in Patagonia (differential of the
#                            ICL, the federal tax on liquid fuels)
#
# The border variables are meant for cross-border arbitrage and the border zone
# of the ICL. d_ciudad_km and d_gba_km capture market size and demand density, as
# covariates of the demand model. patagonia_icl marks the geography of the
# Patagonian tax differential; it does not replicate the department-level tax
# rules. The output is a separate file keyed by nro_inscripcion.

suppressPackageStartupMessages({library(data.table); library(sf); library(rnaturalearth)})
source("code/00_config.R")
sf::sf_use_s2(TRUE)   # geodesic distances

DIR_INT <- DIR_INTERIM
FIN   <- file.path(DIR_INT, "geocodificacion_final.csv")
OUT_G <- file.path(DIR_INT, "variables_geograficas_estacion.csv")
norm <- function(x) toupper(trimws(iconv(as.character(x), "", "ASCII//TRANSLIT")))

# 1. Locatable stations (exacta + localidad) ----
g <- fread(FIN, encoding = "UTF-8")
ubic <- g[precision %in% c("exacta","localidad") & !is.na(lat) & !is.na(lon)]
cat("locatable stations:", nrow(ubic), "\n")
pe <- st_as_sf(ubic, coords = c("lon","lat"), crs = 4326)

# 2a. International border as a line ----
# Edge of the five neighboring countries; a secondary continuous variable. In the
# east and center the nearest border is the Río de la Plata or the Río Uruguay,
# which cannot be crossed by car, so the nearest country is not reported here.
# For arbitrage use the crossings in 2b.
vecinos <- c("Chile","Bolivia","Paraguay","Brazil","Uruguay")
nb <- ne_countries(country = vecinos, scale = "large", returnclass = "sf")
nbu <- st_union(nb)
ubic[, d_frontera_km := round(as.numeric(st_distance(pe, nbu))/1000, 1)]

# 2b. Nearest border crossing open to road traffic ----
# The relevant variable for arbitrage: 22 international road crossings, including
# the bridges to Uruguay.
cruces <- data.table(
  cruce = c("Cristo Redentor","Cardenal Samoré","Pino Hachado","Integración Austral",
            "Jama","San Francisco","Agua Negra","Pehuenche","Huemules","Río Turbio",   # Chile (10)
            "La Quiaca","Salvador Mazza","Aguas Blancas",                              # Bolivia (3)
            "Clorinda","Posadas-Encarnación",                                          # Paraguay (2)
            "Puerto Iguazú","Paso de los Libres","Santo Tomé","Bernardo de Irigoyen",  # Brazil (4)
            "Gualeguaychú-Fray Bentos","Colón-Paysandú","Concordia-Salto"),            # Uruguay (3)
  pais = c(rep("Chile",10), rep("Bolivia",3), rep("Paraguay",2), rep("Brasil",4), rep("Uruguay",3)),
  lat = c(-32.82,-40.72,-38.66,-52.13,-23.23,-26.92,-30.17,-35.98,-45.65,-51.57,
          -22.10,-22.05,-22.72, -25.28,-27.38, -25.60,-29.75,-28.55,-26.25,
          -33.10,-32.22,-31.28),
  lon = c(-70.05,-71.87,-70.90,-69.52,-67.00,-68.29,-69.83,-70.40,-71.65,-72.33,
          -65.60,-63.69,-64.35, -57.72,-55.87, -54.58,-57.09,-56.03,-53.64,
          -58.32,-58.14,-57.90))
pcx <- st_as_sf(cruces, coords = c("lon","lat"), crs = 4326)
ix  <- st_nearest_feature(pe, pcx)
ubic[, d_cruce_km  := round(as.numeric(st_distance(pe, pcx[ix, ], by_element = TRUE))/1000, 1)]
ubic[, cruce_cercano := cruces$cruce[ix]]
ubic[, pais_cruce    := cruces$pais[ix]]
cat("d_frontera_km (border line): median", round(median(ubic$d_frontera_km)), "km\n")
cat("d_cruce_km (road crossing): median", round(median(ubic$d_cruce_km)), "km |",
    "within 25 km of a crossing:", ubic[d_cruce_km < 25, .N], "stations\n")
print(ubic[, .N, by = pais_cruce][order(-N)])

# 3. Large cities ----
# Cities above roughly 100,000 inhabitants; coordinates of the urban center.
ciu <- data.table(
  ciudad = c("Buenos Aires","Córdoba","Rosario","La Plata","Mar del Plata","Tucumán",
             "Salta","Santa Fe","San Juan","Resistencia","Neuquén","Santiago del Estero",
             "Corrientes","Posadas","Bahía Blanca","Paraná","Mendoza","Formosa","Jujuy",
             "La Rioja","Río Cuarto","Comodoro Rivadavia","San Luis","Catamarca",
             "Concordia","San Nicolás","Río Gallegos","Ushuaia","Viedma","Trelew"),
  lat = c(-34.61,-31.42,-32.95,-34.92,-38.00,-26.82,-24.79,-31.63,-31.54,-27.45,-38.95,
          -27.80,-27.47,-27.37,-38.72,-31.73,-32.89,-26.18,-24.19,-29.41,-33.12,-45.86,
          -33.30,-28.47,-31.39,-33.34,-51.62,-54.80,-40.81,-43.25),
  lon = c(-58.38,-64.19,-60.66,-57.95,-57.56,-65.22,-65.41,-60.70,-68.54,-58.99,-68.06,
          -64.26,-58.83,-55.90,-62.27,-60.52,-68.84,-58.17,-65.30,-66.85,-64.35,-67.50,
          -66.34,-65.78,-58.02,-60.21,-69.22,-68.30,-63.00,-65.31))
pc <- st_as_sf(ciu, coords = c("lon","lat"), crs = 4326)
idx <- st_nearest_feature(pe, pc)
ubic[, d_ciudad_km := round(as.numeric(st_distance(pe, pc[idx, ], by_element = TRUE))/1000, 1)]
ubic[, ciudad_cercana := ciu$ciudad[idx]]
gba <- st_as_sf(data.frame(lat=-34.6037, lon=-58.3816), coords=c("lon","lat"), crs=4326)
ubic[, d_gba_km := round(as.numeric(st_distance(pe, gba))/1000, 0)]
cat("\nd_ciudad_km: median", round(median(ubic$d_ciudad_km)), "km |",
    "d_gba_km: median", round(median(ubic$d_gba_km)), "km\n")

# 4. Patagonia flag (Patagonian differential of the ICL) ----
PATAGONIA <- c("NEUQUEN","RIO NEGRO","CHUBUT","SANTA CRUZ","TIERRA DEL FUEGO")
ubic[, patagonia_icl := as.integer(norm(provincia) %in% PATAGONIA)]
cat("stations in Patagonia (ICL):", ubic[patagonia_icl==1, .N], "\n")

# 5. Save ----
out <- ubic[, .(nro_inscripcion, precision, provincia, localidad,
                d_frontera_km, d_cruce_km, cruce_cercano, pais_cruce,
                d_ciudad_km, ciudad_cercana, d_gba_km, patagonia_icl)]
fwrite(out, OUT_G, na = "NA", bom = TRUE)
cat("\n[saved", basename(OUT_G), "-", nrow(out), "stations]\n")
cat("\nsummary:\n")
print(out[, .(d_frontera_km = round(median(d_frontera_km)),
              d_ciudad_km   = round(median(d_ciudad_km)),
              d_gba_km      = round(median(d_gba_km))), by = patagonia_icl])

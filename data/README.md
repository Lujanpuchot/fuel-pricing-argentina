# Data

The panel itself is not here: the raw and intermediate files add up to about 10 GB. This folder holds the small tables the pipeline needs that are not derived from the panel: the locality-to-department crosswalk, which was built once and corrected by hand, and two public reference datasets used by the geocoding. With them, every step except the panel itself runs on a fresh clone.

## Locality to department crosswalk

`crosswalk_localidad_departamento.csv` maps each (province, locality) pair in the station panel to its department. Departments are the markets of the demand model.

| Column | Content |
|---|---|
| `provincia` | Province, as spelled in the source data |
| `localidad` | Locality, as spelled in the source data |
| `departamento` | Department it belongs to |
| `fuente` | How the pair was resolved |

1,398 pairs over 458 distinct (province, department) pairs. All are resolved except one, the placeholder `N/D`, which is left empty on purpose and should be dropped from the model. `fuente` records where each assignment comes from: 1,301 from the georef API of the national government, 58 from the audit of the first version, 34 from a manual dictionary for localities the API does not return, 3 from the City of Buenos Aires (treated as a single market), 1 isolated settlement with a single station and the `N/D` row.

`crosswalk_correcciones_auditoria.csv` (pipe separated) holds the 60 corrections applied after reviewing the first version, one row per locality, with the department assigned and the reason. Most of them repair an indexing bug in that first version, which had sent some localities to an unrelated province.

`06_markets.R` reads the crosswalk from the data folder and only rebuilds it from the API if it is missing or if `REBUILD_FROM_API` is set to `TRUE`. Keeping the file fixed means the market definition does not change if the API does.

Source: [georef](https://apis.datos.gob.ar/georef), the address and administrative geography API of the Argentine government. The file carries no personal data: it is administrative geography plus the locality names as the Energy Secretariat spells them.

## Reference data for the geocoding

Two public datasets, trimmed to the columns the geocoding needs. There is no script that prepares them: they were downloaded once and reduced by hand, so the trimmed versions are kept here.

### `postes_km_dnv.csv`

Kilometre posts of the national highway network, 26,432 of them, used by section 5 of `07_geocode_stations.R` to resolve addresses of the form "Ruta 9 km 412".

| Column | Content |
|---|---|
| `cod_ruta`, `ruta_num` | Highway code and number |
| `progresiva` | Kilometre marked by the post |
| `lat`, `lon` | Coordinates of the post |
| `distrito` | DNV district |

Source: [Postes Kilométricos](https://datos.transporte.gob.ar/dataset/postes-kilometricos), Secretaría de Transporte, surveyed by the Dirección Nacional de Vialidad. Published as open data, license "Otra (Abierta)".

### `coords_oficiales_energia.csv`

The coordinates each operator registered for its outlets, 5,692 of them, used by section 6 as the top layer of the cascade. `idempresa` is the same identifier as `nro_inscripcion` in the panel, so they merge with no geocoding.

| Column | Content |
|---|---|
| `idempresa` | Station identifier |
| `olat`, `olon` | Median registered coordinate |
| `n_obs` | Records the median was taken over |
| `sd_km` | Dispersion of those records, in km |

Source: [Precios en Surtidor, Resolución 314/2016](http://datos.energia.gob.ar/dataset/precios-en-surtidor), Secretaría de Energía, resource "Precios históricos", collapsed to one coordinate per station. Published under Creative Commons Attribution 4.0.

Section 5 and section 6 read these files from the interim folder if they are there, and otherwise from this folder, so the geocoding runs on a fresh clone. Sections 1 to 4 do not need them and already produce a coordinate for every station, at a lower share of exact matches: the two files take exact locations from about 66% to 88.6%.

## The panel

The raw retail files (`public_vi_access_eess_*.rds`, one per period) come from the price and volume reports that outlets file with the Energy Secretariat under Resolution 1104/2004. The rest of the tree is described in the README at the root of the repository.

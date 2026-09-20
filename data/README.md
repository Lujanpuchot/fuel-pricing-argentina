# Data

The panel itself is not here: the raw and intermediate files add up to about 10 GB. This folder holds the two small tables that `06_markets.R` produces and that are worth keeping under version control, because they were built once and then corrected by hand, and because they are useful on their own.

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

## Reference files that have to be downloaded

Two files that `07_geocode_stations.R` uses were downloaded from their public sources and trimmed to the columns below. There is no script that prepares them: get them once and put them in the interim folder (`DIR_INTERIM`, see `code/00_config.R`).

| File | Source | Columns kept |
|---|---|---|
| `postes_km_dnv.csv` | [Kilometre posts](https://datos.transporte.gob.ar/dataset/postes-kilometricos), National Highway Directorate (DNV), layer `poste_1km_2018` | `cod_ruta`, `ruta_num`, `progresiva`, `lat`, `lon` |
| `coords_oficiales_energia.csv` | [Pump prices](http://datos.energia.gob.ar/dataset/precios-en-surtidor), Energy Secretariat, resource "Precios históricos" | `idempresa`, `latitud`, `longitud`, collapsed to the median coordinate per station |

Section 5 of `07_geocode_stations.R` uses the first one to resolve addresses of the form "Ruta 9 km 412", and section 6 uses the second one as the top layer of the cascade. Neither section checks whether its file is there, so both stop with a read error if it is missing. Sections 1 to 4 do not need them and produce a complete set of coordinates on their own, at a lower share of exact matches.

## The panel

The raw retail files (`public_vi_access_eess_*.rds`, one per period) come from the price and volume reports that outlets file with the Energy Secretariat under Resolution 1104/2004. The rest of the tree is described in the README at the root of the repository.

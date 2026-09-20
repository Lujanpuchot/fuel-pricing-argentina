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

1,398 pairs over 457 departments. All are resolved except one, the placeholder `N/D`, which is left empty on purpose and should be dropped from the model.

`fuente` records where each assignment comes from: 1,301 from the georef API of the national government, 58 corrected in the audit of the first version, 34 from a manual dictionary for localities the API does not return, 3 from the City of Buenos Aires (treated as a single market), 1 isolated settlement with a single station and the `N/D` row.

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

## Refining cost from the 20-F filings

`20f_ypf_extract.csv` is one row per fiscal year, 2004 to 2024, read by hand from the annual reports YPF files with the US Securities and Exchange Commission (form 20-F, CIK 0000904851, downloaded from EDGAR). Part 8 of `09_market_data.R` uses it for two layers of the unit cost: the realized price of crude sold in the domestic market and the refining cost per barrel.

| Column | Content |
|---|---|
| `anio_fiscal` | Fiscal year |
| `crudo_realizado_usd_bbl` | Realized price of crude, USD per barrel |
| `refino_cash_cost` | Refining cost per barrel, with its unit in `refino_cash_cost_unidad` |
| `*_label` | The exact wording of the table the figure was taken from |
| `downstream_op_income`, `ventas_refinados_km3`, `crudo_procesado_km3d` | Operating income of the segment, refined sales and crude throughput |
| `filing_url` | The filing on EDGAR, so every figure can be traced |
| `notas` | What had to be decided for that year |

Only ten of the twenty-one years carry a unit refining cost: from 2014 the 20-F reports changes rather than levels, so the series is chained from the 2013 level with the percentages each filing gives. The way the presentation changes over time, and the cases where a filing restates a figure it had published before, are written up in `20f_ypf_notas.md`.

Source: public filings of YPF S.A. with the SEC. The extract and the notes are my own.

## The data tree

`code/00_config.R` expects this layout under `ROOT`. The raw retail files (`public_vi_access_eess_*.rds`, one per period) are the price and volume reports that outlets file with the Energy Secretariat under Resolution 1104/2004; everything under `Intermedio/` is written by the pipeline.

```
Datos/Principal/Minoristas/Última versión/   raw retail files            DIR_RETAIL
Datos/Principal/Minoristas/Última versión/Intermedio/                    DIR_INTERIM
Datos/Principal/Minoristas/Última versión/Intermedio/covariables_mercado/ DIR_COVAR
Datos/Principal/Mayoristas/                  wholesale prices            DIR_WHOLESALE
Documentos/2. Gráficos y tablas descriptivas - new/  tables and figures  DIR_OUTPUT
```

## Inputs the pipeline expects and does not fetch

Beyond the files above, several steps read data that is neither shipped here nor downloaded by the code. They were assembled by hand from public sources. Scripts 1 to 7 do not need any of them.

| File | Needed by | What happens without it |
|---|---|---|
| `georef_departamentos_ref.csv` | `06_markets.R` | The check that every department belongs to its province is skipped, silently |
| `precios_mayoristas_res1104.csv` | `08`, `09` | Dispatch plants and the wholesale margin cannot be built |
| `raw_osm/osm_rutas_troncales.json` | `08` | The on-highway section stops |
| `localizadores_marcas.csv` | `08` | The station amenities section stops |
| `raw_sesco/` | `09` | The downstream section stops |
| `censo2022_vs_proyeccion_depto.csv` | `09` | The census-anchored population variant stops |
| `bio_precios_completado.csv` | `09` | Biofuel prices fall back to a fossil-cost fraction, flagged |
| `raw_series/usgc_gasolina_fob_fred.csv`, `raw_series/dolar_blue_mensual_ambito.csv` | `10` | The price-against-costs figures cannot be drawn |

`09_market_data.R` has two fallbacks that let it finish with a substituted value instead of stopping: the refining cost and the biofuel price. The refining cost no longer falls back, because the extract of the 20-F filings is in this folder; the biofuel one still can, and the `fuente_*` columns of the output record when it happened.

## Glossary

Variable names and data values follow the source, which is in Spanish. These are the terms the code turns on.

| Term | Meaning |
|---|---|
| `boca` (de expendio) | Retail outlet. The unit of observation, identified by `nro_inscripcion` |
| `bandera` | Brand the outlet flies (YPF, Shell, Axion, Puma). `blanca` is an unbranded outlet |
| `nafta` | Gasoline. `Nafta (súper) entre 92 y 95 Ron` is regular, `Nafta (premium) de más de 95 Ron` is premium |
| `gas oil` | Diesel. `Grado 2` is the standard grade, `Grado 3` the premium one |
| `GNC` | Compressed natural gas, sold by the cubic metre and with its own volume scale |
| `GLPA` | Liquefied petroleum gas, reported as its own product family |
| `PRVE` | Part of the source category "Bocas de expendio (venta por menor) Combustibles Líquidos + PRVE". The source does not expand the acronym, and the sales-channel text never contains it, so `05_business_type.R` never re-infers this type |
| `canal_de_comercializacion` | Sales channel. `Al público` is retail; the others are resale and distribution |
| `tipo_negocio` | Type of outlet, inferred from the products it sells when the source gives the generic label |
| `excentos` | Flag for the tax-exempt part of a sale, which the source reports on its own line |
| `no_movimientos` | Flag the source reports per record; it is carried through the panel and compared when deduplicating |
| `tasa_vial`, `ingresos_brutos`, `fondo_fiduciario_gnc` | Road levy, provincial turnover tax and the CNG trust fund, three of the taxes reported per record |
| `precio_surtidor` | Posted pump price. `precio_sin_impuestos` is the same price net of taxes, which is the one used |
| `forma_vertical` | Who operates the outlet, taken from the brands' own station locators: the `RED_PROPIA` field for YPF (company-operated against dealer) and the `MOSO` field for Axion, whose values are DODO, CORS and CODO. It is the observed counterpart of the contract type the model needs |
| `localidad`, `departamento`, `provincia` | Locality, department and province. Departments are the markets of the model |
| `precision` | Quality of a geocoded coordinate: `exacta`, `localidad`, `departamento` or `sin_dato` |

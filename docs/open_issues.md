# Open issues

What is known to be wrong or undecided, with the evidence for each and the rule I am considering. The cleaning in `code/` is deliberately narrow: it removes what is corrupt at any date and leaves anything that needs a judgment call to the point where the estimation sample is built. This file is where those calls are parked, and each one is also an open issue in the repository.

Counts are measured on the analysis panel, retail sales to the public, unless stated otherwise.

## Decisions still to make about the data

### Outlets that report volume in litres

Thirty-two outlets report volumes about a thousand times too large in nearly every month they appear. Their median outlet-month is 83,630, which divided by a thousand is 84: an ordinary outlet. They are 0.4% of outlets and 8.1% of total volume, so they matter for anything built on quantities, market shares above all.

They are distinct from the group below: these report the wrong scale always, not occasionally.

Candidate rule: flag an outlet when more than 90% of its outlet-months exceed a physical ceiling, and divide that outlet's volumes by 1,000 rather than dropping them. Keep a column recording that the outlet was rescaled, so the choice can be undone and its weight in any result can be measured.

### Outlets with occasional volume spikes

Ninety outlets report normal volumes with occasional months in the thousands of cubic metres. They look like bulk deliveries booked into the retail channel rather than a unit problem. In the two focal products, 6,591 outlet-months exceed 3,000 m³, 0.34% of records.

Section 3 of `10_descriptives.R` caps these at 3,000 m³ per outlet-month, which brings annual volume of gasoline and diesel to 10-17.5 million m³ against 222 million uncapped. That cap is descriptive only and drops the rescaling group along with this one.

Candidate rule: once the rescaling above is applied, decide the cap on physical grounds rather than on a percentile, and apply it when the cell is collapsed.

### Prices that are impossible for their year

`03_clean_prices.R` removes prices of zero, at or below 0.01, and above 100,000 pesos per litre. Those cutoffs are absolute over a nominal series whose median goes from 0.99 in 2004 to 191 in 2023, so they only catch what is corrupt at any date. Measured against the median of their own year, 1,552 records exceed it more than twentyfold and 1,383 of those pass the filter, all of them between 2004 and 2009. Prices near 99,000 pesos per litre survive in 2007 and 2010, when a litre cost about 2 pesos.

Candidate rule: add a cutoff relative to the year, as a multiple of the median of the product in that year, and report how many records it removes per year.

### Duplicate records that are not duplicates

After `04_deduplicate.R`, 65,155 records still share an outlet-product-channel-month key. Around three quarters are the taxed and the tax-exempt line of the same sale, which the fuel tax exemption in Patagonia makes common there; most of the rest are copies from the two source files that overlap in 2013. About a fifth are genuinely conflicting reports, almost all diesel in 2009-2011.

Candidate rule: collapse each cell to one record, adding volumes and weighting prices by volume, and keep the exempt share as its own variable rather than as a second line.

### Zero volumes and missing months

The cut at 1e-3 in `02_clean_volume.R` drops every zero along with the near-zeros, so from the analysis panel on, a month in which an outlet sold nothing and a month in which it reported nothing look the same. Section 3 of `10_descriptives.R` measures the gaps this leaves, but the distinction is not recoverable from the panel.

Candidate rule: rebuild the zeros from the pre-cut file when the estimation sample is built, or carry a flag from `02_clean_volume.R`.

### Business type: two rules that do not fire

Two cases in `05_business_type.R` were left as they are, both documented in the script:

- The "since first appearance" rule types 1,289 outlet-months at 36 outlets as liquid fuels only, although in those months they sell only compressed natural gas. The monthly rule would call them CNG only.
- The PRVE branch never runs, because no sales channel in the panel contains that string, so the type "Combustibles Líquidos + PRVE" is never inferred even though the source reports it on 25,517 records.

## Open questions in the geographic variables

### Highway addresses on provincial routes

Section 5 of `07_geocode_stations.R` places addresses of the form "Ruta 9 km 412" with the kilometre posts of the national highway network. The parser keeps the number and drops the class, so an address on provincial route 11 is looked up among the posts of national route 11, which is a different road. Of the 1,392 addresses that carry a kilometre, 53 say provincial, 270 say national and 1,069 say neither. The gate that rejects a new point more than 80 km from the current one catches the absurd cases, but it cannot act on an outlet that had no coordinate to begin with.

The rule I am considering is to skip the provincial ones, which the address already identifies, and to keep the unmarked ones only when the gate can check them.

### Localities that straddle department boundaries

The pipeline keys a market on (province, locality) and gives each pair one department. Asking georef for the full list of localities of every province returns 52 names that belong to more than one department of their own province; the panel uses 14 of them, covering 70 outlets, 0.8% of the total.

Most are not two towns sharing a name. They are single urban areas of Greater Buenos Aires whose territory crosses a partido boundary: Tortuguitas appears in three, and Del Viso, Gerli, Villa Adelina, Canning and Malvinas Argentinas in two. Those outlets are assigned to one of the partidos they span, which may not be the one they physically sit in. The partidos involved are adjacent and comparable, so the market is a neighbour rather than a stranger.

The rule I am considering is to take the department from the coordinate rather than from the name for outlets with an exact location, which is 88.6% of them, and to leave the name-based assignment for the rest.

## Open questions in the cost series

### The refining cost factor for 2024

From 2014 the 20-F stops reporting refining cost per barrel and gives only changes, so the series is chained from the 2013 level. Three years have no unit figure and are derived from the total cost, adjusted by throughput, and 2024 does it differently from the other two:

```r
"2015" = 1.178 * 46.2 / 47.5               # the whole factor is adjusted
"2020" = 1.182 * 44.1 / 37.4               # the whole factor is adjusted
"2024" = 1 + (189 / 1600) * (46.8 / 47.9)  # only the increment is adjusted
```

Under the rule of 2015 and 2020, 2024 would be about 1.0925 instead of 1.1154. The difference is two points in one year, and because the series is chained it carries into every quarter after it. The asymmetry may be deliberate, since 2024 is derived from an amount in dollars rather than from a published percentage, but the two treatments should be stated and one of them chosen.

## Known issues in the exported tables

These affect the LaTeX output, not the panel.

- Table labels come out doubled: `save_tex_table()` passes `label = "tab:..."` and `kable()` adds its own prefix, so the files carry `\label{tab:tab:...}` and a `\ref{tab:...}` in the thesis will not resolve.
- Table 33 escapes twice. Cells that already contain `\%` go through `kable(escape = TRUE)` and come out as `\textbackslash{}\%`. Table 29 avoids this by using a plain `%`.
- Tables 10 and 11 print empty cells. `dcast` drops business types with no outlets in the last twelve months while the loop runs over every level, so the column for "Otros" shows `()` instead of `0 (0.0%)`.
- `norm_txt()` does not merge operator names that end in punctuation. It trims before turning punctuation into spaces, so "YPF S.A." and "YPF S.A" stay separate operators. This affects the tables built on `operador_norm`.

## Smaller things

- Section 6 of `08_station_variables.R` ends after printing the heading of a CNG cross-check between the locator and the address; the check itself was never written.
- Outlets with no department are still sent to Nominatim in section 1 of `07_geocode_stations.R`. The department is what bounds the search box and what the result is checked against, so for those outlets a match can come back from anywhere in the country and still be labelled exact.
- `03_clean_prices.R` has no flag for negative prices: every low-price flag requires the price to be positive. `diag_precio$n_negativos` reports whether any exist.
- `02_clean_volume.R` writes `eess_all_cleaned1.rds` twice, first with the auxiliary columns and then without, so an interrupted run leaves the wider file under that name.
- Deduplication keeps the record from the file whose name sorts first, which is the older one. Only the substantive variables are compared, so the columns that exist only in the newer files are missing in the record that is kept.
- `variables_espaciales_panel.csv` is built on outlets with an exact coordinate, so the set of rivals is incomplete where coverage is low. `cobertura_exacta` records this per market-year and should be used to weight or to exclude.

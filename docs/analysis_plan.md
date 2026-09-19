# Analysis plan: choices fixed before estimation

Luján Puchot. First version: 18 September 2026.

This document records the choices fixed before estimating the structural model described in [`research_proposal.md`](research_proposal.md). It is not a registered pre-analysis plan for an experiment. The data are observational and I have already worked with them: the station panel has been cleaned, geocoded and merged with market covariates, and the descriptive and event-study analysis has been run. The demand and supply estimation has not been run, so no price coefficient, markup or value of $\lambda$ exists yet. The plan is a dated record, written to limit specification search in the remaining steps.

## Hypotheses

1. Conduct. YPF prices as if it maximized profit plus $\lambda$ times consumer surplus, with $\lambda > 0$: its margin moves less than one for one with the profit-maximizing margin $\mu^{\ast}$ implied by demand, while that of private brands moves one for one.
2. Government, not ownership. YPF's excess weight over private brands changes with the government in office and, in the strong form, is already positive before May 2012, under Repsol.
3. Geography. The weight is larger in departments where YPF is the only brand.

A fourth hypothesis, that the objective also works through the wholesale price charged to YPF's dealers and only when prices are free, was rejected with the wholesale data before estimation: the wholesale gap between YPF and private brands is about 5 percent under regulation (2004–2015) and about 2 percent in free windows. No vertical bargaining model is estimated.

## Outcomes

The primary outcome is, for each government period, the slope of private brands' margins on $\mu^{\ast}$ minus the contract-adjusted slope of YPF's margins, in the same provinces, grades and quarters and with the same cost construction. I will defend its sign and its ordering across periods, not the level of $\lambda$.

Secondary outcomes are the level of $\lambda$ by period (a range, with a sensitivity table by cost layer), the contrast between captive and competitive departments (with a first-order condition aggregated by price zone as a check against near-uniform pricing), the transfer to consumers in pesos per litre by government with a counterfactual at $\lambda = 0$, and event studies around May 2012 and July–November 2018.

## Sample and unit of observation

The sample is retail sales to the public of regular and premium gasoline and grade 2 and grade 3 diesel, December 2004 to December 2024. Gasoline and diesel are separate systems; gasoline is estimated first and carries the main result. In demand the unit is brand × grade × department × quarter, with unbranded stations as one composite brand and price defined as the median across the product's stations. The supply regressions stack four station groups: YPF company-operated (identified by the operator's tax number), other YPF stations, private dealers and private company-operated stations.

Government periods are December 2004–April 2012, May 2012–December 2015, January 2016–July 2019, August–November 2019, December 2019–November 2023 and December 2023–December 2024. The last is descriptive only, because guidance to YPF on pricing is documented for most of its months. Months with documented quantity rationing (July–November 2018, 2022 and part of 2023) are excluded from the period slopes.

## Market definition and market size

A market is a department (457) in a quarter. Market size must not contain the current quarter's quantity. Two or three definitions are carried, among 1.5 times the maximum volume of adjacent years, the same using only past years, and a physical potential built from population, car ownership and consumption per capita. The main one is chosen on the data, not on results: its outside share must be below 0.05 in fewer than 2 percent of cells (the original definition gave zero in about 26 percent).

## Prices and costs

Prices are net of taxes in demand and supply, in constant pesos. Two measures are carried: the net-of-tax price reported by stations, and a reconstruction from the pump price and the statutory schedule of fuel taxes, VAT and regional exemptions. A result that changes sign between them is reported as a band. Rows reporting volume in litres are rescaled and flagged, and the panel must reproduce official national gasoline sales within a ratio of 0.93 to 1.05 in every year from 2010.

Cost is measured, not inferred from a pricing condition, in the layers listed in the proposal. The mean logistics cost comes from an external tariff and affects only the level of $\lambda$. The provincial step is the median deviation of private brands' resale wholesale prices within operator, product and month. Freight per kilometre is never added to it, since wholesale prices already include freight, and distance bands are capped at about 1.4 percent of the price per 100 km. In months in which a firm's imports are at least 5 percent of its sales over a three-month window, its cost is the import unit value plus biofuel (2.5 and 10 percent as sensitivity).

## Estimating equations

Demand is the random-coefficients logit of the proposal, estimated before supply and not jointly, because the pricing conditions are known to fail in regulated months and would contaminate the demand parameters. The price instrument is a refinery supply-shock shifter (deviations of throughput from normal levels, weighted by inverse distance, from 2010); counts of rival products by grade identify the random coefficients. Prices in other markets, wages, wholesale prices and import parity are not used as instruments. The shifter predicts wholesale prices ($t = -4.1$) but not yet retail prices (pooled reduced-form $F$ of 2.6 to 4.2). The criterion is an effective first-stage $F$ above 10, overall and by period, with the expected sign; otherwise the instrument question is declared open.

The pricing conditions are $p - c - \kappa = (1-\lambda_G)\mu^{\ast}$ for YPF company-operated stations, $(1-\tau)p - c = (1-\tau-\lambda_G)\mu^{\ast}$ for consignment and $p - w - \kappa = \mu^{\ast}$ for private dealers. YPF's resale stations are excluded; the retail panel does not record each dealer's contract, so consignment and resale shares from the wholesale data bound the results for the other YPF stations. The stacked regression is

$$
m_{jmt} = \phi_{\text{province} \times \text{grade} \times t} + x_{jmt}'\delta + b_{kG}\,\hat{\mu}^{\ast}_{jmt} + \varepsilon_{jmt},
$$

with $m$ the margin of the corresponding condition and $x$ containing local wages, volume, highway or urban location and distance bands, which control for freight and $\kappa$ without valuing them. Then $\lambda_G = 1 - b$ for company-operated stations and $\lambda_G = 1 - \tau - b$ for consignment, and YPF's main estimate pools both under a common $\lambda_G$. Private slopes are estimated by period, not imposed.

Months from January 2016 to July 2019 and from December 2023 are classified as free or not in a table with a source per month, completed before estimation. In binding months the condition is an inequality, $\lambda_{\text{firm}} \le 1 - \text{margin}/\mu^{\ast}$, so the firm's own preference is only bounded from above.

## Inference

Standard errors are clustered by province × quarter. $\mu^{\ast}$ is a generated regressor measured with error, which attenuates slopes and inflates $\lambda$; it is instrumented, and a two-step bootstrap carries the uncertainty of the demand step. Attenuation would also push the private-dealer slope below one, so the placebo detects it. In a preliminary check with a logit proxy for the markup, the first stage on local market structure was strong ($F$ of 55 to 245 by period), yet private brands showed an implied $\lambda$ of 0.76 to 0.93. The gate is therefore the placebo, not the first-stage $F$. A conduct test in the sense of Duarte, Magnolfi, Sølvsten and Sullivan (2024, full reference in the proposal) is run by period, conditional on the placebo.

## Falsification tests

| Test | Result if the model is right |
|---|---|
| Private dealers, wholesale price observed | Slope of one. A prior regression of $p - w$ on the dealer's $\mu^{\ast}$ checks who sets dealers' prices; if the brand does (coefficient near zero), the placebo is rewritten as the brand's condition |
| Months classified as free (cleanest today: first quarter of 2019) | $\lambda$ close to zero for every group |
| July–November 2018, estimated separately | $\lambda > 0$ for YPF only |
| Private company-operated stations, refiner-specific constructed cost (Pampas provinces and City of Buenos Aires only) | Slope of one |
| YPF company-operated versus consignment | Company-operated $\lambda$ inside the interval implied by the two commission values |
| YPF company-operated and consignment pooled | A common $\lambda$ per period is not rejected |
| Private wholesale price minus unit cost, monthly | Positive. Preliminary: with the basket factor, 53 of 482 grade-months remain negative, all but six in 2008 or earlier; those six are in diesel |

## Robustness bands

Results are reported over the full range of each band, with sampling error on top.

| Uncertain input | Band | Disciplined by |
|---|---|---|
| Consignment commission | Contractual 9–10 percent to measured wedge 16–20 percent | YPF company-operated stations, which pay none |
| Station operating cost $\kappa$ | Zero to the dealer's gross margin (19–23 percent over the wholesale price) | Consignment stations, whose condition has no $\kappa$ |
| Within-province freight | Zero to the empirical ceiling | Distance bands as controls |
| Provincial step applied to YPF | Private brands' step to the prediction from YPF's own network | Mirror equation; YPF's wholesale prices by locality outside 2012–2015 |
| Market size in captive markets | Two or three definitions | Affects a secondary outcome only |
| Net price | Reported to reconstructed | YPF company-operated stations, where the two fields agree most |
| Unit cost (barrel allocation, crude, refining) | Sensitivity by layer | Mirror equation, wholesale margin test, placebo |

## Evidence against the main hypothesis

The main hypothesis fails if YPF's excess weight is zero in every period while the placebo passes: the discount would then reflect cost or demand. The government reading fails if the excess jumps in May 2012 and is flat within each ownership regime. If the placebo fails, or the positive control is not detected, the comparison is declared uninformative and I will say so instead of re-specifying. If the sign or ordering of the excess changes inside any band, the result is reported as not robust. A weight that is uniform or lower in captive departments reads as a near-national price, not as a mandate.

## Departures from the plan

Any departure (another market-size definition, a changed sample, an instrument added or dropped, a reclassified month) will be listed in a dated section at the end of this file, with the reason and, where computable, the results under the original choice.

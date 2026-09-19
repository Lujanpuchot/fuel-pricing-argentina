# State ownership and retail fuel pricing in Argentina: research proposal

Luján Puchot. Master's thesis in Economics, Universidad de San Andrés. Work in progress.

## Research question

YPF is Argentina's largest fuel company: it sells a little over half of the gasoline and diesel bought at the pump, and close to a third of service stations carry its brand. It was controlled by Repsol until May 2012 and has been 51 percent state-owned since. Descriptive evidence from station-level prices for 2004–2024, still preliminary, shows that YPF charges less than private brands in the same local market, and that the discount follows the government in office more closely than the ownership of the firm. A price gap alone cannot separate lower costs or a different demand from a choice to charge less than market position allows. This thesis tries to measure that choice. I ask whether the state used YPF to hold down fuel prices, under which governments, how much margin the company gave up, and how much of it reached consumers.

In the model YPF maximizes profit plus $\lambda$ times consumer surplus, so its margin is $(1-\lambda)$ times the profit-maximizing margin, and $\lambda$ is estimated by government period. The main result is not the level of $\lambda$, which depends on cost components that are hard to measure, but YPF's excess weight relative to private brands selling in the same markets under the same cost shocks.

## Institutional background

Law 26,741 of May 2012 expropriated 51 percent of YPF's shares from Repsol. The company remained listed in New York, and its annual report to the US Securities and Exchange Commission warns that the controlling shareholder's decisions may differ from the interests of other shareholders, including on the pricing of its main products.

Until 2015 pump prices were administered de facto, through informal agreements and pressure from the Secretaría de Comercio Interior (Domestic Trade Secretariat) under the Ley de Abastecimiento (Supply Law). Prices were freed in October 2017, frozen for two months by agreement in May 2018, and frozen again, together with domestic crude, by Decree 566/2019 in August 2019. Another freeze ran from December 2019 to August 2020, and until 2023 increases were negotiated with the refiners, from late 2022 under the Precios Justos (Fair Prices) programme with monthly caps of about 4 percent. Decree 70/2023 repealed the Supply Law in December 2023, but press reports document guidance from the Economy Ministry to YPF during 2024, so I do not treat that year as one of free pricing.

Contracts between refiners and stations determine who sets the pump price. Oil companies operate about 8 percent of stations directly; YPF does so through its subsidiary OPESSA, which runs roughly a tenth of its network. Most other YPF stations work on consignment: YPF owns the fuel until it is sold, sets the pump price and pays the dealer a percentage commission. Other brands sell fuel to their dealers at a wholesale price and, by contract, the dealer sets the pump price. Unbranded stations, about 30 percent of outlets, have no exclusivity contract.

Fuel taxes vary across space. Gasoline sold in Patagonia and a few adjacent jurisdictions is exempt from the impuesto a los combustibles líquidos (ICL, the federal tax on liquid fuels), and diesel pays a reduced amount (Law 23,966, Article 7(d); the area and the treatment by product were redefined in 2015 and 2018). In my data taxes are about 18 percent of the pump price of regular gasoline in Patagonia and 33 percent elsewhere in 2018–2024 (preliminary), which conditions how net-of-tax prices and regional wholesale costs are built.

## Data

The main source is the monthly price and volume report that service stations file with the Secretaría de Energía (Energy Secretariat) under Resolution 1104/2004. The cleaned panel has 5.05 million observations by outlet, product, sales channel and month, from December 2004 to December 2024, for 9,214 outlets in all 24 provinces. I geocoded the 8,720 stations that sell to the public (99.7 percent have coordinates, 88.6 percent at exact precision) and mapped localities to the 457 departments that define local markets. The same resolution generates a wholesale dataset by operator, product, province, month and channel, which separates resale to dealers, consignment and company-operated stations; it gives the wholesale price paid by private dealers and what YPF keeps on consignment sales. The Secretariat's downstream statistics add, from 2010, sales by firm and province, refinery throughput by plant and imports by firm. Cost inputs come from crude prices reported for royalty purposes, YPF's annual reports and regulated biofuel prices.

## Empirical model

Demand is a random-coefficients logit in the tradition of Berry, Levinsohn and Pakes (1995), estimated following Conlon and Gortmaker (2020). A market is a department in a quarter, and a product is a brand × grade pair (regular and premium gasoline, grade 2 and grade 3 diesel), with unbranded stations grouped into one composite brand. Gasoline and diesel are separate systems, and gasoline is estimated first. Utility depends on price, the brand's number of outlets in the department, and product and quarter fixed effects, with random coefficients on price and the premium grade. Price sensitivity varies with local income and across five regions, the regional approach of Culós, Gabrielli and Herrera Gómez (2022).

On the supply side, multiproduct firms compete in prices. Private firms maximize profit. YPF maximizes profit plus $\lambda$ times consumer surplus, a linear objective in the spirit of Matsumura (1998), with consumer surplus in place of total welfare. Since a marginal price increase lowers consumer surplus by the quantity sold, YPF's first-order conditions are those of a private firm with quantities scaled by $(1-\lambda)$:

$$
p - c = (1-\lambda)\,\mu^{\ast},
$$

where $\mu^{\ast}$ is the multiproduct Bertrand margin implied by demand at observed prices, which does not depend on anyone's conduct. With $\lambda = 0$ YPF behaves like a private firm; with $\lambda = 1$ it prices at marginal cost.

Marginal cost is usually recovered by inverting this condition under an assumed conduct. When conduct is the object that does not work: any margin YPF gives up would appear as an implausibly low inferred cost, and $\lambda$ would be zero by construction. I measure cost outside the model, in layers: the realized domestic price of crude allocated across products, refining cost from YPF's annual reports, the mandated biofuel blend at regulated prices, import cost in months in which a firm imported at least 5 percent of its sales, and a provincial step taken from private brands' wholesale prices. Import parity is not used as cost, because regulation kept domestic crude below world prices in some years and above them in others.

The pricing condition that applies to a station is that of whoever sets its price, as in the literature on contractual form in gasoline retailing (Shepard 1993; Hastings 2004). At YPF's company-operated stations, $p - c - \kappa = (1-\lambda)\mu^{\ast}$, where $\kappa$ is the cost of running the station. On consignment YPF pays the dealer a share $\tau$ of the price, so $(1-\tau)p - c = (1-\tau-\lambda)\mu^{\ast}$. There $\kappa$ drops out, but $\tau$ is uncertain, between a contractual commission of 9 to 10 percent and a total wedge of 16 to 20 percent measured in the wholesale data, which bundles the commission with other payments. YPF's resale stations are excluded. Private dealers provide a placebo, $p - w - \kappa = \mu^{\ast}$ with the wholesale price $w$ observed, and private company-operated stations a mirror equation that tests the constructed cost.

## Identification and main comparisons

The supply regressions stack station groups $k$ and government periods $G$:

$$
m_{jmt} = \phi_{\text{province} \times \text{grade} \times t} + x_{jmt}'\delta + b_{kG}\,\hat{\mu}^{\ast}_{jmt} + \varepsilon_{jmt},
$$

where $m$ is the margin relevant for the contract and $x$ includes local wages, volume, a highway indicator and distance bands. The model implies $b = 1$ for private dealers, $1-\lambda_G$ for YPF's company-operated stations and $1-\tau-\lambda_G$ on consignment. Identification comes from how margins move with $\mu^{\ast}$ across departments within a province, grade and quarter. The fixed effects absorb cost components common to that cell, which makes the slope far less sensitive to cost mismeasurement than the level.

The main result is YPF's excess weight, the private slope minus the contract-adjusted YPF slope, in each government period: Repsol-controlled YPF (December 2004 to April 2012), state-controlled YPF under Fernández de Kirchner (to 2015), Macri (to July 2019), the freeze of August to November 2019, and Fernández (to November 2023); 2024 is reported descriptively. Private brands are not assumed to have $\lambda = 0$ throughout, since administered prices compress their margins too. The level of $\lambda$ is reported only as a range. In months in which a price path binds, a price at the cap is consistent with a high $\lambda$ and with constrained profit maximization, so only an upper bound on the firm's own preference is identified, in the spirit of Dubois and Lasio (2018). A final step expresses the estimates in pesos per litre transferred to consumers and simulates $\lambda = 0$ with private brands re-optimizing.

Two pieces of event evidence do not need the demand model. The discount does not start with the nationalization of May 2012: it was present while YPF was private, and the gaps between YPF and private brands in wholesale and pump prices narrowed sharply between February and April 2012, before the change of ownership (preliminary; for regular gasoline the pump gap went from about 20 to about 6 percent). The second episode is July to November 2018, after the freeze agreed in May, when press reports document that YPF kept prices below import parity and supplied without quotas while private brands rationed deliveries. It is a positive control: the model should find $\lambda > 0$ for YPF only.

The falsification tests are listed in the analysis plan. The central ones are that private dealers must show a slope of one and that $\lambda$ should be close to zero for every group in months classified as free before estimation. Every uncertain input carries a band, and the main result is claimed only if it holds across all of them.

## Expected contribution and related literature

The idea that a public firm can regulate an oligopoly from within goes back to Merrill and Schneider (1966), but empirical counterparts are few. The closest paper is Duarte, Magnolfi and Roncoroni (2025), who test whether Italian consumer cooperatives internalize consumer surplus, under the same parametrization of the objective, and conclude that they maximize profit. Unlike them, I estimate the weight instead of testing a grid of values, I measure cost outside the model, and the panel spans five government periods and one change of ownership.

On method, the thesis belongs to the conduct-parameter tradition of Bresnahan (1982). Corts (1999) warns that such a parameter measures how margins respond to demand shifters and not the level of market power, which is one reason to estimate $\lambda$ by period and to add the conduct test of Duarte, Magnolfi, Sølvsten and Sullivan (2024). YPF uses few price zones, so a lower margin in captive markets could reflect the near-uniform pricing that DellaVigna and Gentzkow (2019) document for retail chains; the contrast between captive and competitive markets is therefore a secondary result.

For Argentina, Coloma (2002) finds that gasoline pricing moved from Cournot competition to price leadership by Repsol-YPF after their 1999 merger. Culós, Gabrielli and Herrera Gómez (2022) estimate wholesale demand and markups during the free-pricing window of 2016–2019 and find that YPF has the highest markups in all four products; Garay (2020) estimates a BLP model of retail gasoline for 2017. Both assume that every firm maximizes profit, as is natural with a single regime. I study the retail market at department level over twenty years of administered and free prices, and estimate the objective of one firm instead of assuming it.

## Current status and next steps

The data work is largely done: cleaning of the station panel, geocoding, market covariates, a first version of the cost series, a regulatory chronology, and the descriptive and event analysis. In that analysis (preliminary), YPF's pump price for regular gasoline is 3.6 to 4.7 percent below that of private brands in the same department and month until 2015 and 0.5 to 1.6 percent below afterwards. In episodes with an announced price path, 90 to 93 percent of YPF outlets sit exactly on the authorized number, against 72 to 82 percent of private branded outlets.

The demand and supply models have not been estimated. Before estimating demand I am correcting three measurement problems found in validation checks: a small share of rows report volume in litres instead of cubic metres, which shows up against official sales; the reported net-of-tax price is inconsistent with the pump price in 23 to 42 percent of observations and is being rebuilt from the pump price and the statutory tax schedule; and the original market-size definition leaves a zero outside share in about a quarter of cells. The relevance of the price instrument for retail prices also remains to be shown. Next come the event studies with corrected data, gasoline demand in PyBLP, the supply regressions, and welfare. The choices fixed before estimation are recorded in [`analysis_plan.md`](analysis_plan.md).

## References

Berry, S., J. Levinsohn and A. Pakes (1995). "Automobile Prices in Market Equilibrium." *Econometrica* 63(4): 841–890.

Bresnahan, T. F. (1982). "The Oligopoly Solution Concept Is Identified." *Economics Letters* 10(1–2): 87–92.

Coloma, G. (2002). "The Effect of the Repsol-YPF Merger on the Argentine Gasoline Market." *Review of Industrial Organization* 21(4): 399–418.

Conlon, C. and J. Gortmaker (2020). "Best Practices for Differentiated Products Demand Estimation with PyBLP." *RAND Journal of Economics* 51(4): 1108–1161.

Corts, K. S. (1999). "Conduct Parameters and the Measurement of Market Power." *Journal of Econometrics* 88(2): 227–250.

Culós, M. T. V., M. F. Gabrielli and M. Herrera Gómez (2022). "Market Power in the Argentine Liquid Fuels Wholesale Chain." RedNIE Working Paper 157. Later version: Gabrielli, F., V. Culós, M. Herrera Gómez and M. Willington (2024), Asociación Argentina de Economía Política, Working Paper 4732.

DellaVigna, S. and M. Gentzkow (2019). "Uniform Pricing in U.S. Retail Chains." *Quarterly Journal of Economics* 134(4): 2011–2084.

Duarte, M., L. Magnolfi and C. Roncoroni (2025). "The Competitive Conduct of Consumer Cooperatives." *RAND Journal of Economics* 56(1): 106–125.

Duarte, M., L. Magnolfi, M. Sølvsten and C. Sullivan (2024). "Testing Firm Conduct." *Quantitative Economics* 15(3): 571–606.

Dubois, P. and L. Lasio (2018). "Identifying Industry Margins with Price Constraints: Structural Estimation on Pharmaceuticals." *American Economic Review* 108(12): 3685–3724.

Garay, M. E. (2020). "Estimación BLP del poder de mercado en la comercialización de naftas." Master's thesis in Economics, Universidad de San Andrés.

Hastings, J. S. (2004). "Vertical Relationships and Competition in Retail Gasoline Markets: Empirical Evidence from Contract Changes in Southern California." *American Economic Review* 94(1): 317–328.

Matsumura, T. (1998). "Partial Privatization in Mixed Duopoly." *Journal of Public Economics* 70(3): 473–483.

Merrill, W. C. and N. Schneider (1966). "Government Firms in Oligopoly Industries: A Short-Run Analysis." *Quarterly Journal of Economics* 80(3): 400–412.

Shepard, A. (1993). "Contractual Form, Retail Price, and Asset Characteristics in Gasoline Retailing." *RAND Journal of Economics* 24(1): 58–77.

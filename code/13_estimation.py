# 13_estimation.py
# Demand estimation and the markups the supply side needs.
#
# Input:  estimation_sample_nafta.csv, written by 12_estimation_sample.R
# Output: demand_results.txt  the estimated demand parameters
#         markups.csv         one markup per product and market, for the supply step
#
# This is the estimation code, not estimated results. Demand has not been run on
# the full sample yet, and the supply step at the end is waiting on the cost file
# described in section 3. Run it on a few hundred markets first: the contraction
# is solved market by market, and 35,481 of them take hours rather than minutes.
#
# Demand is estimated before supply and not jointly. The pricing conditions fail
# by construction in the months when prices were set by agreement, and estimating
# the two sides together would let that failure move the demand parameters, which
# are what the whole exercise rests on.

import numpy as np
import pandas as pd
import pyblp

pyblp.options.verbose = True

SAMPLE = "estimation_sample_nafta.csv"
N_MARKETS = None   # None for all of them; a number while the specification is being tried out
SPATIAL_IV = True  # see section 1: buys sharper instruments at the cost of 17% of the markets


# 1. Sample and instruments ----------------------------------------------------

products = pd.read_csv(SAMPLE)

# 11_demand_sample.R and 12_estimation_sample.R already write the columns PyBLP
# expects under the names it expects: market_ids, product_ids, firm_ids, shares
# and prices. A product is a brand and a grade, a market is a department and a
# quarter, and shares are of the potential market, so they sum to less than one.
if N_MARKETS is not None:
    keep = products["market_ids"].drop_duplicates().head(N_MARKETS)
    products = products[products["market_ids"].isin(keep)].copy()

# The instrument for price is a refinery supply shock: deviations of throughput
# from its normal level at each refinery, weighted by the inverse distance from
# the market to it, so that a market close to a refinery running below capacity
# faces a worse supply position for reasons that have nothing to do with how much
# its drivers want to buy. The weights need the monthly throughput series and the
# distances from 08_station_variables.R; the file is not built yet.
#
# Three things that look like instruments here are deliberately not used. Prices
# in other markets move with the same national wholesale price, so they are not
# excluded. Wages enter demand through income and are a demand shifter, not a
# supply one. The wholesale price is the object the supply side is about: using
# it to instrument the retail price would assume away the question.
try:
    shock = pd.read_csv("refinery_shock_by_market.csv")
    products = products.merge(shock, on="market_ids", how="left", validate="m:1")
    products["demand_instruments0"] = products["refinery_shock"]
    n_own = 1
except FileNotFoundError:
    # Without it there is no excluded cost shifter, and price has to lean on the
    # instruments below, which move substitution rather than the cost of serving
    # a market. They identify the random coefficients well and the price level
    # badly, so what comes out of this branch is a specification that runs, not
    # an elasticity to report.
    print("No refinery shock file: price is identified only off the rival instruments.")
    n_own = 0

# Counts of rival products identify the random coefficients: what a second brand
# in the market does to shares depends on how much substitution there is, and the
# counts move with entry, which happens years before, rather than with the local
# demand shock of the quarter. They are constant within a market by construction,
# so they enter in levels and their work is across markets.
counts = ["n_otras_marcas", "n_productos_mercado"]

# The spatial instruments are sharper, because how far the nearest rival pump is
# says more about substitution than how many rivals the department holds, and
# unlike the counts they differ between the brands inside a market: the distance
# to the nearest rival varies within 72% of markets and the count of rivals
# within five kilometres within 62%. That within-market variation is what
# differentiation instruments are built from.
#
# They are missing wherever the stations were geocoded to a centroid rather than
# to an address, since a distance between two centroids is not a distance. Shares
# have to add up within a market, so a market with a missing instrument leaves
# whole rather than one product at a time, and that costs 6,126 of the 35,481
# markets, 14.1% of the rows and 7.6% of the volume. What it costs are the small
# markets, which are also the concentrated ones and the ones the question is
# about, so this is not a neutral trade and both versions are worth running.
spatial = ["d_rival_min", "n_riv_5km"]

if SPATIAL_IV:
    complete = products.groupby("market_ids")[spatial].transform(lambda x: x.notna().all())
    incomplete = ~complete.all(axis=1)
    if incomplete.any():
        lost = products.loc[incomplete, "market_ids"].nunique()
        print(f"Dropping {lost} markets ({incomplete.mean():.1%} of rows) for a missing instrument.")
        products = products[~incomplete].copy()
    local = pyblp.build_differentiation_instruments(
        pyblp.Formulation("0 + " + " + ".join(spatial)), products
    )
else:
    local = np.empty((len(products), 0))

# Distance to the nearest refinery is not used, although it shifts cost and
# varies between brands within a market. A market far from every refinery is also
# a market far from everything else, and remoteness moves income and population,
# which are demand shifters. It stays a control in the supply equation, where it
# stands in for freight without having to be priced.
instruments = np.c_[products[counts].to_numpy(), local]
for i in range(instruments.shape[1]):
    products[f"demand_instruments{n_own + i}"] = instruments[:, i]


# 2. Demand --------------------------------------------------------------------

# The linear part holds price and a full set of product fixed effects, so a brand
# and grade is compared with itself across markets and quarters rather than with
# another brand. Quarter effects take out everything national: the exchange rate,
# the crude price, the tax schedule and the months of price agreements.
#
# The random coefficients are on the constant and on price. The one on the
# constant is what separates the brands people are willing to drive past from the
# outside option; the one on price is what makes substitution depend on how much
# a household cares about the price rather than on shares alone, which is the
# whole reason for not using plain logit here. Both are needed for the markups in
# section 3 to mean anything, since a logit markup is a function of the share.
X1 = pyblp.Formulation("1 + prices", absorb="C(product_ids) + C(trimestre)")
X2 = pyblp.Formulation("1 + prices")

problem = pyblp.Problem(
    (X1, X2),
    products,
    integration=pyblp.Integration("product", size=7),
)
print(problem)

results = problem.solve(
    sigma=np.diag([0.5, 0.2]),
    optimization=pyblp.Optimization("l-bfgs-b", {"gtol": 1e-5}),
)
print(results)

with open("demand_results.txt", "w") as f:
    f.write(str(problem) + "\n\n" + str(results) + "\n")

# The price coefficient has to be negative and the implied own-price elasticities
# have to be below -1: a firm setting prices never chooses an inelastic point on
# its demand curve, so an elasticity above -1 means the demand estimates are
# wrong, not that the firm is unusual.
elasticities = results.extract_diagonal_means(results.compute_elasticities())
print("Mean own-price elasticity:", elasticities.mean())


# 3. Markups, and the weight on consumer surplus -------------------------------

# eta is the markup a firm maximising profit would charge, given the demand just
# estimated and who owns what: the multiproduct Bertrand term, which PyBLP builds
# from firm_ids. It is a function of demand alone and does not need costs.
mu_star = results.compute_eta()

markups = products[["market_ids", "product_ids", "firm_ids", "prices", "shares"]].copy()
markups["mu_star"] = mu_star
markups.to_csv("markups.csv", index=False)

# What lambda is, and why this stops here.
#
# A company-operated YPF station that puts weight lambda on consumer surplus
# rather than on profit alone prices at
#
#     p - c - kappa = (1 - lambda) * mu_star
#
# with c the wholesale cost of the litre and kappa the cost of moving and selling
# it. A private dealer sets p - w - kappa = mu_star. So lambda is read off the
# distance between the margin a firm actually takes and the margin the same
# demand system says a profit maximiser would take, and it is identified by the
# comparison between the two kinds of station in the same market and quarter,
# which is the stacked regression in docs/analysis_plan.md.
#
# The missing piece is c. Everything else on the left is observed: p is in the
# panel and the private dealer's w is in the wholesale data. c for YPF is not,
# because YPF refines its own crude, and a transfer price inside a vertically
# integrated firm is an accounting entry rather than a market price. It is
# measured rather than inferred, since inferring it from a pricing condition is
# assuming the answer, and it is measured in layers:
#
#   crude       the domestic wellhead price, held below Brent for most of these
#               years, times the yield of the refining process
#   refining    an operating cost per litre from the 20-F filings in data/
#   blending    the mandated biofuel share at the published biodiesel and
#               bioethanol prices
#   logistics   an external freight tariff, which moves the level of lambda but
#               not the comparison between firms
#
# 09_market_data.R builds these layers. What is not settled is the yield and the
# refining cost, and both move c by enough to move lambda.
#
# Until they are settled, lambda is bounded rather than estimated. c is a cost,
# so it is at least zero and at most the wholesale price a private dealer pays in
# the same market and month, which YPF would not exceed to supply itself. Putting
# those two ends into the pricing condition gives an interval for lambda that
# holds whatever the refining cost turns out to be, and the point estimate is
# what the interval collapses to once c is measured. In the months when prices
# were set by agreement the condition is an inequality in any case and only
# bounds lambda from above, so those months support the interval and nothing
# sharper.

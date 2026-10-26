"""Demand for gasoline (or diesel): random-coefficients logit in PyBLP.

Reads the product-market file written by 12_estimation_sample.R and writes the
demand estimates and, for the supply step, the Bertrand markup of every product
in every market.

    python code/13_estimation.py                    # gasoline, every market
    python code/13_estimation.py --markets 500      # a random 500, to try a change
    python code/13_estimation.py --optimal-iv       # re-solve with optimal instruments
    python code/13_estimation.py --system gasoil

Output, in <interim>/blp_<system>/ unless --out says otherwise:
    estimates.csv     coefficients of the logit, the random-coefficients model
                      and, with --optimal-iv, the re-estimate
    diagnostics.json  sample, instruments, first stage, convergence, elasticities
    markups.csv       one row per product and market: price, share, own
                      elasticity, diversion to the outside good and mu_star
    summary.txt       PyBLP's own printout of each problem and its results
    results.pkl       the final results, to reload without solving again

Prices are net of taxes, in thousands of pesos of the base quarter (2024T4 by
default). The contraction runs market by market and the full sample takes
hours, so a change is tried on --markets first.

Demand is estimated before supply and not jointly: the pricing conditions fail
in the months under price agreements, and a joint estimate would let that move
the demand parameters. How mu_star enters the supply side is in
docs/analysis_plan.md.
"""

from __future__ import annotations

import argparse
import contextlib
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
import pyblp

import demand_data as dd

log = logging.getLogger("estimation")

PRICE_SCALE = 1000.0  # prices enter in thousands of pesos

# Linear part: price, the number of outlets the brand runs in the department
# (how much of the brand a driver actually meets), and product and quarter
# fixed effects. A product is compared with itself across markets, and the
# quarter effects absorb everything national: exchange rate, crude, the tax
# schedule and the price agreements.
X1 = pyblp.Formulation("0 + prices + outlets", absorb="C(product_ids) + C(trimestre)")

# Random coefficients on price and on the high grade. The first lets
# substitution depend on price sensitivity and not only on shares, which is
# what gives the markups content; the second lets premium buyers move to
# another premium product before they move down a grade.
#
# The proposal also lets price sensitivity vary with income. That needs a
# distribution of households per market and the department files only have a
# mean wage, so for now income enters nothing.
X2 = pyblp.Formulation("0 + prices + premium")

# Starting values for sigma (price, premium). The GMM objective is not convex
# in sigma; the problem is solved from each and the lowest objective among the
# runs that converged is kept.
STARTS = [(0.5, 0.2), (1.5, 0.5), (3.0, 1.0)]

OPTIMIZATION = pyblp.Optimization("l-bfgs-b", {"gtol": 1e-6})
SOLVE = dict(optimization=OPTIMIZATION, W_type="clustered", se_type="clustered")

# Rival counts by grade and in the market. They move with entry, which is
# decided years before, and not with the demand shock of the quarter.
COUNTS = ["n_otras_marcas", "n_productos_mercado", "n_mismo_grado"]

# Station-level distances, averaged over the outlets of each product. They
# vary between brands inside a market, which is what differentiation
# instruments are built from. They are missing where outlets were geocoded to
# a locality centroid, and a market with a gap leaves whole: about a sixth of
# the markets, mostly small and concentrated ones. --no-spatial keeps them.
SPATIAL = ["d_rival_min", "n_riv_5km"]


def parse_args(argv=None):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--system", choices=["nafta", "gasoil"], default="nafta")
    p.add_argument("--input", type=Path,
                   help="estimation sample (default: <interim>/estimation_sample_<system>.csv)")
    p.add_argument("--out", type=Path, help="output folder (default: <interim>/blp_<system>)")
    p.add_argument("--markets", type=int, help="estimate on a random subsample of markets")
    p.add_argument("--seed", type=int, default=1)
    p.add_argument("--base", default="2024T4", help="quarter whose pesos prices are expressed in")
    p.add_argument("--price-band", type=float, nargs=2, default=(0.5, 2.0),
                   metavar=("LOW", "HIGH"),
                   help="drop markets with a price outside LOW-HIGH times the grade-quarter median")
    p.add_argument("--no-spatial", action="store_true", help="leave out the spatial instruments")
    p.add_argument("--optimal-iv", action="store_true",
                   help="re-solve with approximate optimal instruments")
    p.add_argument("--integration-size", type=int, default=7,
                   help="Gauss-Hermite nodes per dimension of the product rule")
    p.add_argument("--processes", type=int, default=1)
    p.add_argument("--verbose", action="store_true", help="show PyBLP's progress")
    return p.parse_args(argv)


# Sample and instruments --------------------------------------------------------

def prepare(args, interim: Path) -> tuple[pd.DataFrame, dict]:
    path = args.input or interim / f"estimation_sample_{args.system}.csv"
    products = dd.load_sample(path)
    report = {"input": str(path), "markets_read": int(products["market_ids"].nunique()),
              "rows_read": len(products)}

    cpi = dd.quarterly_cpi(interim / "covariables_mercado" / "covar_series_nacionales.csv")
    products = dd.deflate(products, cpi, base=args.base, scale=PRICE_SCALE)

    products, report["price_screen"] = dd.screen_prices(products, *args.price_band)
    log.info("price screen: %d markets dropped", report["price_screen"]["markets_dropped"])

    if not args.no_spatial:
        products, report["markets_without_spatial"] = dd.complete_markets(products, SPATIAL)
        log.info("spatial instruments: %d markets dropped", report["markets_without_spatial"])

    products = dd.sample_markets(products, args.markets, args.seed)
    dd.check_shares(products)

    products["premium"] = products["grade"].str.contains("premium|Grado 3", case=False).astype(int)
    products["clustering_ids"] = products["provincia"] + "|" + products["trimestre"]
    products = products.sort_values(["market_ids", "product_ids"]).reset_index(drop=True)
    return products, report


def add_instruments(products: pd.DataFrame, interim: Path, system: str,
                    spatial: bool) -> tuple[pd.DataFrame, list[str]]:
    """Excluded instruments, stored as demand_instruments0, 1, ... for PyBLP.

    Not used, for the reasons in docs/analysis_plan.md: prices in other
    markets, wages, wholesale prices and import parity. Distance to the nearest
    refinery is not used either: remote markets are also poorer and smaller,
    so it shifts demand as well as cost.
    """
    blocks, names = [], []

    # The price instrument: refinery throughput shocks weighted by inverse
    # distance. It starts in 2010, so with it the sample starts in 2010 too.
    shock_file = interim / f"refinery_shock_{system}.csv"
    if shock_file.exists():
        shock = pd.read_csv(shock_file, encoding="utf-8-sig")[["market_ids", "refinery_shock"]]
        products = products.merge(shock, on="market_ids", how="left", validate="m:1")
        products = products[products["refinery_shock"].notna()].reset_index(drop=True)
        blocks.append(products[["refinery_shock"]].to_numpy(float))
        names.append("refinery_shock")
    else:
        log.warning("%s not found: price rests on the rival and spatial instruments, "
                    "which identify substitution better than the price level", shock_file.name)

    blocks.append(products[COUNTS].to_numpy(float))
    names += COUNTS

    if spatial:
        local = pyblp.build_differentiation_instruments(
            pyblp.Formulation("0 + " + " + ".join(SPATIAL)), products)
        blocks.append(local)
        names += [f"differentiation_{i + 1}" for i in range(local.shape[1])]

    z = np.column_stack(blocks)
    varies = z.std(axis=0) > 0
    for name, v in zip(names, varies):
        if not v:
            log.warning("instrument %s is constant in this sample and is left out", name)
    z, names = z[:, varies], [n for n, v in zip(names, varies) if v]

    products = products.drop(columns=[c for c in products if c.startswith("demand_instruments")])
    for i in range(z.shape[1]):
        products[f"demand_instruments{i}"] = z[:, i]
    return products, names


# Estimation ----------------------------------------------------------------------

def solve_logit(products: pd.DataFrame):
    problem = pyblp.Problem(X1, products)
    return problem, problem.solve(W_type="clustered", se_type="clustered")


def solve_rc(products: pd.DataFrame, size: int):
    problem = pyblp.Problem((X1, X2), products,
                            integration=pyblp.Integration("product", size=size))
    runs = []
    for start in STARTS:
        results = problem.solve(sigma=np.diag(start), **SOLVE)
        runs.append(results)
        log.info("start %s: objective %.6g, converged %s, sigma %s", start,
                 float(results.objective), results.converged,
                 np.round(np.diag(results.sigma), 4))
    converged = [r for r in runs if r.converged]
    if not converged:
        log.warning("no start converged; keeping the lowest objective anyway")
    best = min(converged or runs, key=lambda r: float(r.objective))
    return problem, best, runs


def solve_optimal(results):
    """Re-solve with the approximation to the optimal instruments of
    Conlon and Gortmaker (2020), starting from the first estimate."""
    problem = results.compute_optimal_instruments(method="approximate").to_problem()
    return problem, problem.solve(results.sigma, method="1s", **SOLVE)


# Output --------------------------------------------------------------------------

def coefficients(results, model: str) -> pd.DataFrame:
    rows = [(model, "beta", lab, b, se) for lab, b, se in
            zip(results.beta_labels, results.beta.ravel(), results.beta_se.ravel())]
    rows += [(model, "sigma", lab, s, se) for lab, s, se in
             zip(results.sigma_labels, np.diag(results.sigma), np.diag(results.sigma_se))]
    return pd.DataFrame(rows, columns=["model", "parameter", "label", "estimate", "se"])


def own_elasticities(results) -> np.ndarray:
    return results.extract_diagonals(results.compute_elasticities()).ravel()


def convergence(results) -> dict:
    return {
        "converged": bool(results.converged),
        "objective": float(results.objective),
        "projected_gradient_norm": float(results.projected_gradient_norm),
        "reduced_hessian_eigenvalues": np.asarray(results.reduced_hessian_eigenvalues).ravel().tolist(),
        "optimization_iterations": int(results.optimization_iterations),
    }


def main(argv=None) -> None:
    args = parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(message)s", datefmt="%H:%M:%S")
    pyblp.options.verbose = args.verbose

    interim = dd.interim_dir(dd.data_root())
    out = args.out or interim / f"blp_{args.system}"
    out.mkdir(parents=True, exist_ok=True)

    products, report = prepare(args, interim)
    products, instruments = add_instruments(products, interim, args.system, not args.no_spatial)
    report.update(markets=int(products["market_ids"].nunique()), rows=len(products),
                  firms=int(products["firm_ids"].nunique()),
                  products=int(products["product_ids"].nunique()))
    log.info("sample: %d markets, %d rows", report["markets"], report["rows"])

    excluded = [f"demand_instruments{i}" for i in range(len(instruments))]
    first = dd.first_stage(products, "prices", ["outlets"], excluded,
                           absorb=["product_ids", "trimestre"], cluster="clustering_ids")
    log.info("first stage: effective F %.1f over %d instruments",
             first["effective_f"], first["instruments_used"])

    parallel = pyblp.parallel(args.processes) if args.processes > 1 else contextlib.nullcontext()
    with parallel:
        logit_problem, logit = solve_logit(products)
        logit_own = own_elasticities(logit)

        problem, results, runs = solve_rc(products, args.integration_size)
        summary = [str(logit_problem), str(logit), str(problem), str(results)]
        tables = [coefficients(logit, "logit"), coefficients(results, "rc")]
        final = "rc"

        if args.optimal_iv:
            problem_opt, results = solve_optimal(results)
            summary += [str(problem_opt), str(results)]
            tables.append(coefficients(results, "rc_optimal"))
            final = "rc_optimal"

        own = own_elasticities(results)
        outside = results.extract_diagonals(results.compute_diversion_ratios()).ravel()
        aggregate = results.compute_aggregate_elasticities(factor=0.1).ravel()
        mu_star = results.compute_eta().ravel()

    # A price-setting firm never sits on the inelastic part of its demand, so
    # own elasticities above -1 point to a problem with the estimates.
    diagnostics = {
        "sample": report,
        "price_units": f"thousands of {args.base} pesos per litre, net of taxes",
        "instruments": instruments,
        "first_stage": first,
        "logit": {
            "price_coefficient": float(logit.beta[0, 0]),
            "median_own_elasticity": float(np.median(logit_own)),
            "share_inelastic": float(np.mean(logit_own > -1)),
        },
        "starts": [{"sigma0": list(s), "sigma": np.diag(r.sigma).tolist(),
                    "objective": float(r.objective), "converged": bool(r.converged)}
                   for s, r in zip(STARTS, runs)],
        "final_model": final,
        "convergence": convergence(results),
        "elasticities": {
            "median_own": float(np.median(own)),
            "mean_own": float(np.mean(own)),
            "share_inelastic": float(np.mean(own > -1)),
            "median_aggregate_10pct": float(np.median(aggregate)),
            "median_outside_diversion": float(np.median(outside)),
        },
    }
    log.info("median own elasticity %.2f (logit %.2f); %.1f%% of products inelastic",
             diagnostics["elasticities"]["median_own"], diagnostics["logit"]["median_own_elasticity"],
             100 * diagnostics["elasticities"]["share_inelastic"])

    markups = products[["market_ids", "product_ids", "firm_ids", "grade", "provincia",
                        "trimestre", "price_nominal", "shares"]].copy()
    markups["price"] = products["prices"] * PRICE_SCALE
    markups["own_elasticity"] = own
    markups["outside_diversion"] = outside
    markups["mu_star"] = mu_star * PRICE_SCALE

    pd.concat(tables).to_csv(out / "estimates.csv", index=False)
    markups.to_csv(out / "markups.csv", index=False)
    (out / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2), encoding="utf-8")
    (out / "summary.txt").write_text("\n\n".join(summary) + "\n", encoding="utf-8")
    results.to_pickle(out / "results.pkl")
    log.info("written to %s", out)


if __name__ == "__main__":
    main()

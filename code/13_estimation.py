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
                      elasticity, diversion to the outside good and mu_star;
                      price and mu_star in pesos of the base quarter per
                      litre, price_nominal in current pesos
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
import math
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
# in sigma, so the problem is solved from each; see solve_rc for how one is
# picked.
STARTS = [(0.5, 0.2), (1.5, 0.5), (3.0, 1.0)]

OPTIMIZATION = pyblp.Optimization("l-bfgs-b", {"gtol": 1e-6})
SOLVE = dict(optimization=OPTIMIZATION, W_type="clustered", se_type="clustered")

# L-BFGS-B also reports success when a line search steps into a region where
# the contraction fails and it falls back to the last point, which need not be
# a minimum. A run counts as converged only if the projected gradient is small
# where it stopped. PyBLP multiplies the objective, and so the gradient, by the
# number of observations, so the bound is per observation.
GRADIENT_TOL = 1e-6

# Rival counts by grade and in the market. They move with entry, which is
# decided years before, and not with the demand shock of the quarter.
COUNTS = ["n_otras_marcas", "n_productos_mercado", "n_mismo_grado"]

# Station-level distances, averaged over the outlets of each product. They
# vary between brands inside a market, which is what differentiation
# instruments are built from. They are missing when no outlet of the product
# has an exact coordinate, and a market with a gap leaves whole: 3,828 of the
# 34,027 gasoline markets that pass the price screen. --no-spatial keeps them.
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
        # PyBLP's column order: the own-firm count of close products for each
        # characteristic, then the rival count
        names += [f"{who}_close_{c}" for who in ("own", "rival") for c in SPATIAL]
        assert len(names) == sum(b.shape[1] for b in blocks)

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


def stationary(results) -> bool:
    if results.theta.size == 0:  # nothing left to optimize over
        return True
    gradient = float(results.projected_gradient_norm) / results.problem.N
    return bool(results.converged) and gradient < GRADIENT_TOL


def solve_rc(products: pd.DataFrame, size: int):
    """Random-coefficients logit from each of STARTS, two-step from the best.

    The starts are compared on the first GMM step, whose weighting matrix
    depends only on the instruments. Second-step objectives are not comparable:
    each uses a weighting matrix built from its own first step.
    """
    problem = pyblp.Problem((X1, X2), products,
                            integration=pyblp.Integration("product", size=size))
    runs = []
    for start in STARTS:
        r = problem.solve(sigma=np.diag(start), method="1s", **SOLVE)
        runs.append(r)
        log.info("start %s: objective %.6g, gradient per observation %.2g, sigma %s", start,
                 float(r.objective), float(r.projected_gradient_norm) / problem.N,
                 np.round(np.diag(r.sigma), 4))
    usable = [i for i, r in enumerate(runs) if stationary(r)]
    if not usable:
        log.warning("no start reached a stationary point; going on from the lowest objective")
    best = min(usable or range(len(runs)), key=lambda i: float(runs[i].objective))
    # At sigma = 0 the gradient is zero too, so a run that collapsed there
    # always looks stationary. Say so if a run that did not stop cleanly got lower.
    if any(float(r.objective) < float(runs[best].objective) for r in runs):
        log.warning("a start that did not reach a stationary point has a lower objective "
                    "than the one kept; try other starting values")
    results = problem.solve(sigma=np.diag(STARTS[best]), **SOLVE)
    return problem, results, runs, best


def solve_optimal(results):
    """Re-solve with the approximation to the optimal instruments of
    Conlon and Gortmaker (2020), starting from the first estimate.

    A random coefficient estimated at or next to its lower bound of zero has an
    optimal instrument that is zero, or nearly, everywhere, and with it the
    weighting matrix cannot be inverted. Below 1e-4, which is zero for any
    purpose with prices in thousands of pesos and a 0-1 premium dummy, sigma is
    held at zero and its column left out. drop_indices replaces PyBLP's
    default, which drops the columns of exogenous linear coefficients in theta;
    there are none, because the linear coefficients are concentrated out.
    """
    optimal = results.compute_optimal_instruments(method="approximate")
    sigma = results.sigma.copy()
    labels = list(results.theta_labels)
    drop = []
    for k, name in enumerate(results.sigma_labels):
        label = f"{name} x {name}"
        if label in labels and abs(sigma[k, k]) < 1e-4:
            log.warning("sigma on %s is at zero (%.2g) and is held there", name, sigma[k, k])
            sigma[k, k] = 0.0
            drop.append(labels.index(label))
    if len(drop) == len(labels):
        log.warning("every sigma is held at zero: the re-estimate is a logit with optimal instruments")
    problem = optimal.to_problem(drop_indices=drop or None)
    return problem, problem.solve(sigma, method="1s", **SOLVE)


# Output --------------------------------------------------------------------------

def coefficients(results, model: str) -> pd.DataFrame:
    rows = [(model, "beta", lab, b, se) for lab, b, se in
            zip(results.beta_labels, results.beta.ravel(), results.beta_se.ravel())]
    rows += [(model, "sigma", lab, s, se) for lab, s, se in
             zip(results.sigma_labels, np.diag(results.sigma), np.diag(results.sigma_se))]
    return pd.DataFrame(rows, columns=["model", "parameter", "label", "estimate", "se"])


def own_elasticities(results) -> np.ndarray:
    return results.extract_diagonals(results.compute_elasticities()).ravel()


def upward_sloping(results) -> float:
    """Share of consumers whose price coefficient, beta + sigma * v with v
    standard normal, is positive."""
    b = float(results.beta[list(results.beta_labels).index("prices"), 0])
    k = list(results.sigma_labels).index("prices")
    s = abs(float(results.sigma[k, k]))
    return float(b > 0) if s == 0 else 0.5 * (1 + math.erf(b / (s * math.sqrt(2))))


def convergence(results) -> dict:
    gradient = float(results.projected_gradient_norm)
    return {
        "converged": bool(results.cumulative_converged),
        "stationary": stationary(results),
        "objective": float(results.objective),
        "projected_gradient_norm": gradient if np.isfinite(gradient) else None,
        "reduced_hessian_eigenvalues": np.asarray(results.reduced_hessian_eigenvalues).ravel().tolist(),
        "optimization_iterations": int(results.cumulative_optimization_iterations),
        "markets_contraction_failed": int((results.fp_converged[:, -1] == 0).sum()),
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
    names = dict(zip(excluded, instruments))
    for key in ("coefficients", "standard_errors"):
        first[key] = {names[k]: v for k, v in first[key].items()}
    first["instruments_dropped"] = [names[k] for k in first["instruments_dropped"]]
    log.info("first stage: effective F %.1f over %d instruments",
             first["effective_f"], first["instruments_used"])

    parallel = pyblp.parallel(args.processes) if args.processes > 1 else contextlib.nullcontext()
    with parallel:
        logit_problem, logit = solve_logit(products)
        logit_own = own_elasticities(logit)

        problem, results, runs, best = solve_rc(products, args.integration_size)
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
        # PyBLP gives the change in the inside share per unit of the factor;
        # divided by the inside share it is the elasticity of total demand,
        # an arc elasticity for a 10% rise in every price of the market.
        inside = products.groupby("market_ids")["shares"].sum()
        inside = inside.reindex(results.problem.unique_market_ids.ravel()).to_numpy()
        aggregate = results.compute_aggregate_elasticities(factor=0.1).ravel() / inside
        # Bertrand markup in price units, p - c, with c backed out of the
        # first-order conditions under the ownership in firm_ids
        mu_star = products["prices"].to_numpy() - results.compute_costs().ravel()

    upward = upward_sloping(results)
    nonpositive = float(np.mean(mu_star <= 0))
    if logit.beta[0, 0] >= 0 or nonpositive > 0:
        log.warning("logit price coefficient %.3g, %.1f%% of consumers with demand sloping up, "
                    "%.1f%% of markups not positive: the markups cannot be used as they are",
                    logit.beta[0, 0], 100 * upward, 100 * nonpositive)

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
                    "objective_first_step": float(r.objective),
                    "projected_gradient_norm": float(r.projected_gradient_norm),
                    "stationary": stationary(r)}
                   for s, r in zip(STARTS, runs)],
        "chosen_start": list(STARTS[best]),
        "final_model": final,
        "convergence": convergence(results),
        "elasticities": {
            "median_own": float(np.median(own)),
            "mean_own": float(np.mean(own)),
            "share_inelastic": float(np.mean(own > -1)),
            "median_aggregate_10pct": float(np.median(aggregate)),
            "median_outside_diversion": float(np.median(outside)),
        },
        "markups": {
            "median_mu_star": float(np.median(mu_star) * PRICE_SCALE),
            "share_not_positive": nonpositive,
            "share_consumers_upward_sloping": upward,
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

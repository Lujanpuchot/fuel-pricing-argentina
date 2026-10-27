"""Data preparation for the demand estimation in 13_estimation.py.

Kept apart from the estimation so that it can be tested without PyBLP. Every
function works on the product-market file written by 12_estimation_sample.R:
one row per market (department x quarter) and product (brand x grade).
"""

from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd

REQUIRED = [
    "market_ids", "product_ids", "firm_ids", "grade", "prices", "shares",
    "outlets", "provincia", "trimestre",
]


# Paths -----------------------------------------------------------------------

def data_root() -> Path:
    """The data folder, looked up as in code/00_config.R."""
    candidates = [
        os.environ.get("FUEL_DATA_ROOT", ""),
        str(Path.home() / "Dropbox" / "Trabajo" / "Tesis"),
    ]
    for c in candidates:
        if c and Path(c).is_dir():
            return Path(c)
    raise FileNotFoundError(
        "Data folder not found. Set FUEL_DATA_ROOT to the folder holding the data tree."
    )


def interim_dir(root: Path) -> Path:
    return root / "Datos" / "Principal" / "Minoristas" / "Última versión" / "Intermedio"


# Sample ----------------------------------------------------------------------

def load_sample(path: Path) -> pd.DataFrame:
    products = pd.read_csv(path, encoding="utf-8-sig")
    missing = [c for c in REQUIRED if c not in products.columns]
    if missing:
        raise ValueError(f"{path.name} lacks {missing}")
    return products


def quarterly_cpi(path: Path) -> pd.Series:
    """Quarterly mean of the spliced CPI, indexed like trimestre ("2010T3")."""
    s = pd.read_csv(path, encoding="utf-8-sig", usecols=["mes", "ipc_empalmado"])
    mes = pd.to_datetime(s["mes"])
    quarter = mes.dt.year.astype(str) + "T" + mes.dt.quarter.astype(str)
    return s.groupby(quarter)["ipc_empalmado"].mean()


def deflate(products: pd.DataFrame, cpi: pd.Series, base: str = "2024T4",
            scale: float = 1000.0) -> pd.DataFrame:
    """Express prices in `scale` pesos of the base quarter.

    The nominal price is kept in price_nominal. Measuring price in thousands of
    pesos only changes the units of its coefficient, but it keeps sigma on
    price of the same order as the other parameters, which the optimizer needs.
    """
    if base not in cpi.index:
        raise KeyError(f"no CPI for the base quarter {base}")
    level = products["trimestre"].map(cpi)
    if level.isna().any():
        gaps = sorted(products.loc[level.isna(), "trimestre"].unique())
        raise ValueError(f"no CPI for {gaps[:5]}")
    out = products.copy()
    out["price_nominal"] = out["prices"]
    out["prices"] = out["price_nominal"] * cpi[base] / level / scale
    return out


def screen_prices(products: pd.DataFrame, low: float = 0.5,
                  high: float = 2.0) -> tuple[pd.DataFrame, dict]:
    """Drop markets with a product priced far from its grade and quarter.

    A price below `low` or above `high` times the median of its grade in the
    same quarter is a reporting error (docs/open_issues.md, issue 3). With a
    random coefficient on price, one wrong price enters the inversion of every
    share in its market, so the whole market goes.
    """
    median = products.groupby(["trimestre", "grade"])["prices"].transform("median")
    ratio = products["prices"] / median
    bad = ~ratio.between(low, high)
    markets = products.loc[bad, "market_ids"].unique()
    kept = products[~products["market_ids"].isin(markets)].copy()
    return kept, {"band": [low, high], "rows_out_of_band": int(bad.sum()),
                  "markets_dropped": int(len(markets))}


def complete_markets(products: pd.DataFrame, columns: list[str]) -> tuple[pd.DataFrame, int]:
    """Keep the markets in which every product has all of `columns`."""
    ok = products[columns].notna().all(axis=1)
    ok = ok.groupby(products["market_ids"]).transform("all")
    lost = products.loc[~ok, "market_ids"].nunique()
    return products[ok].copy(), int(lost)


def sample_markets(products: pd.DataFrame, n: int | None, seed: int) -> pd.DataFrame:
    """A random subsample of n markets, all of their products included."""
    markets = products["market_ids"].unique()
    if n is None or n >= len(markets):
        return products
    keep = np.random.default_rng(seed).choice(markets, size=n, replace=False)
    return products[products["market_ids"].isin(keep)].copy()


def check_shares(products: pd.DataFrame) -> None:
    """Shares must be positive and leave a positive outside share."""
    if (products["shares"] <= 0).any():
        raise ValueError("non-positive shares in the sample")
    inside = products.groupby("market_ids")["shares"].sum()
    if (inside >= 1).any():
        raise ValueError(f"{int((inside >= 1).sum())} markets with no outside share")


# First stage -----------------------------------------------------------------

def demean(x: np.ndarray, groups: list[np.ndarray], tol: float = 1e-10,
           max_iter: int = 500) -> np.ndarray:
    """Remove several sets of fixed effects by alternating projections.

    Subtracts the group means of one set at a time until a full pass changes
    nothing. With a single set this is the usual within transformation.
    """
    x = np.array(x, dtype=float, copy=True)
    if x.ndim == 1:
        x = x[:, None]
    codes = [pd.factorize(g)[0] for g in groups]
    counts = [np.bincount(c) for c in codes]
    for _ in range(max_iter):
        change = 0.0
        for c, n in zip(codes, counts):
            for j in range(x.shape[1]):
                means = np.bincount(c, weights=x[:, j]) / n
                x[:, j] -= means[c]
                change = max(change, np.abs(means).max())
        if change < tol * (1 + np.abs(x).max()):
            return x
    raise RuntimeError("demeaning did not converge")


def first_stage(products: pd.DataFrame, endog: str, exog: list[str],
                excluded: list[str], absorb: list[str], cluster: str) -> dict:
    """Strength of the excluded instruments for price.

    Price, the included exogenous variables and the excluded instruments are
    demeaned by the absorbed fixed effects and the exogenous variables are
    partialled out. Reports the effective F of Montiel Olea and Pflueger (2013)
    and the robust Wald F, both with a cluster-robust variance and without
    small-sample corrections. With homoskedastic errors both equal the usual
    first-stage F. The coefficients and their standard errors are there to
    read the sign of each instrument.
    """
    m = demean(products[[endog] + exog + excluded].to_numpy(float),
               [products[g].to_numpy() for g in absorb])
    y, x, z = m[:, 0], m[:, 1:1 + len(exog)], m[:, 1 + len(exog):]
    if x.shape[1]:
        coef = np.linalg.lstsq(x, np.c_[y, z], rcond=None)[0]
        resid = np.c_[y, z] - x @ coef
        y, z = resid[:, 0], resid[:, 1:]

    # An instrument that the fixed effects absorb, or that repeats a combination
    # of the ones before it, carries nothing and is left out. Variation is
    # measured against the instrument's own spread before demeaning, because
    # alternating projections leave a residual of the order of the tolerance.
    before = products[excluded].to_numpy(float).std(axis=0)
    keep = []
    for j in np.flatnonzero(z.std(axis=0) > 1e-6 * before):
        if np.linalg.matrix_rank(z[:, keep + [j]]) == len(keep) + 1:
            keep.append(j)
    z = z[:, keep]

    zz = z.T @ z
    pi = np.linalg.solve(zz, z.T @ y)
    u = y - z @ pi
    g = pd.factorize(products[cluster])[0]
    scores = np.column_stack([np.bincount(g, weights=z[:, j] * u) for j in range(z.shape[1])])
    bread = np.linalg.inv(zz)
    v = bread @ (scores.T @ scores) @ bread
    used = [excluded[j] for j in keep]
    return {
        "effective_f": float(pi @ zz @ pi / np.trace(v @ zz)),
        "robust_wald_f": float(pi @ np.linalg.solve(v, pi) / len(pi)),
        "coefficients": dict(zip(used, pi.tolist())),
        "standard_errors": dict(zip(used, np.sqrt(np.diag(v)).tolist())),
        "instruments_used": len(used),
        "instruments_dropped": [e for e in excluded if e not in used],
        "clusters": int(g.max() + 1),
    }

"""Refinery supply shock, the price instrument of the demand estimation.

For each market, the crude run in the quarter by the refineries that can supply
it, relative to their normal level, with each refinery discounted by its
distance to the department:

    z_mt = sum_r q_rt / d_mr  /  sum_r qbar_rt / d_mr  -  1

q_rt is the crude processed by refinery r in quarter t, qbar_rt the mean of the
same quarter in the year before and the year after (the rule market size
follows in 11_demand_sample.R) and d_mr the distance in km from the department
centroid to the refinery. A refinery thus counts in proportion to its normal
runs over its distance, so a small plant next door does not outweigh La Plata.
z is zero in a normal quarter and negative when nearby refineries ran less
than usual; the expected sign in the first stage is negative.

    python code/refinery_shock.py
    python code/refinery_shock.py --system gasoil

Reads the markets of the estimation sample, covar_sesco_refinacion.csv from
09_market_data.R and centroides_departamento.csv from 07_geocode_stations.R.
Writes refinery_shock_<system>.csv, one row per market, which 13_estimation.py
uses when it finds it. Runs are reported from January 2010, so earlier markets
get no shock and leave the sample with it.
"""

from __future__ import annotations

import argparse
import logging
from pathlib import Path

import numpy as np
import pandas as pd

import demand_data as dd

log = logging.getLogger("refinery_shock")

# The eight refineries that supplied the domestic market, at the coordinates of
# section 2 of 08_station_variables.R, with the names the SESCO table gives
# them. Names are matched exactly: "Dock Sud-DAPSA", "Campana-ENARSA" or
# "Plaza Huincul-NAO" are other, small plants. San Lorenzo changed hands twice
# and ran its last crude in February 2018. Everything left out is 1.3% of the
# crude processed since 2010.
REFINERIES = {
    "La Plata":      (-34.86, -57.90, ["La Plata"]),
    "Luján de Cuyo": (-33.03, -68.88, ["Luján de Cuyo"]),
    "Plaza Huincul": (-38.93, -69.20, ["Plaza Huincul"]),
    "Dock Sud":      (-34.65, -58.34, ["Dock Sud"]),
    "Campana":       (-34.17, -58.96, ["Campana"]),
    "Bahía Blanca":  (-38.75, -62.27, ["Elicâbe"]),
    "Campo Durán":   (-22.20, -63.70, ["Campo Duran"]),
    "San Lorenzo":   (-32.72, -60.75, ["San Lorenzo", "San Lorenzo - OIL", "San Lorenzo - Bravo"]),
}

# georef splits the city of Buenos Aires into comunas and returns no centroid
# for it, so centroides_departamento.csv leaves it empty.
CITY_CENTROID = {("CAPITAL FEDERAL", "CABA"): (-34.61, -58.44)}

EARTH_RADIUS_KM = 6371.0


def parse_args(argv=None):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--system", choices=["nafta", "gasoil"], default="nafta")
    p.add_argument("--input", type=Path,
                   help="estimation sample (default: <interim>/estimation_sample_<system>.csv)")
    p.add_argument("--out", type=Path,
                   help="output file (default: <interim>/refinery_shock_<system>.csv)")
    return p.parse_args(argv)


def crude_runs(path: Path) -> pd.DataFrame:
    """Crude processed by refinery and quarter, in m3, quarters in rows.

    Crude is every input by basin plus imported crude. A refinery that reports
    other inputs in a month but no crude did not run crude that month, and San
    Lorenzo ran none after it closed: both count as zero, not as missing. Only
    quarters with all three months in the table are kept.
    """
    r = pd.read_csv(path, encoding="utf-8-sig")
    crude = r["concepto"].str.startswith("Cuenca") | (r["concepto"] == "Crudo importado")
    r = r[(r["bloque"] == "carga") & crude]
    name = {s: ref for ref, (_, _, names) in REFINERIES.items() for s in names}
    r = r.assign(refinery=r["refineria"].map(name)).dropna(subset=["refinery"])
    r["trimestre"] = r["anio"].astype(str) + "T" + ((r["mes"] - 1) // 3 + 1).astype(str)

    months = r.groupby("trimestre")["mes"].nunique()
    r = r[r["trimestre"].isin(months.index[months == 3])]
    q = r.pivot_table(index="trimestre", columns="refinery", values="cantidad_m3",
                      aggfunc="sum", fill_value=0.0)
    return q.reindex(columns=list(REFINERIES), fill_value=0.0).sort_index()


def normal_runs(q: pd.DataFrame) -> pd.DataFrame:
    """Mean of the same quarter in the year before and the year after, or the
    one of the two that is in the table."""
    year = q.index.str[:4].astype(int)
    k = q.index.str[4:]

    def shifted(s):
        return q.reindex((year + s).astype(str) + k).set_axis(q.index)

    return (shifted(-1) + shifted(1)).div(2).fillna(shifted(-1)).fillna(shifted(1))


def distances(lat, lon) -> np.ndarray:
    """Great-circle distance in km from each point to each refinery."""
    ref = np.array([v[:2] for v in REFINERIES.values()])
    la1, lo1 = np.radians(np.asarray(lat, float))[:, None], np.radians(np.asarray(lon, float))[:, None]
    la2, lo2 = np.radians(ref[:, 0])[None, :], np.radians(ref[:, 1])[None, :]
    h = np.sin((la2 - la1) / 2) ** 2 + np.cos(la1) * np.cos(la2) * np.sin((lo2 - lo1) / 2) ** 2
    return 2 * EARTH_RADIUS_KM * np.arcsin(np.sqrt(h))


def market_centroids(markets: pd.DataFrame, centroids: pd.DataFrame) -> pd.DataFrame:
    """Attach the centroid of each market's department.

    12_estimation_sample.R merges the departments that share an INDEC code into
    one market labelled "A + B"; it gets the mean of the centroids of its parts.
    """
    centroids = centroids.set_index(["provincia", "departamento"])
    for key, xy in CITY_CENTROID.items():
        if key in centroids.index and centroids.loc[key, ["clat", "clon"]].isna().all():
            centroids.loc[key, ["clat", "clon"]] = xy
    centroids = centroids.reset_index()

    parts = markets.assign(part=markets["departamento"].str.split(" + ", regex=False)).explode("part")
    parts = parts.merge(centroids.rename(columns={"departamento": "part"}),
                        on=["provincia", "part"], how="left")
    xy = parts.groupby("market_ids")[["clat", "clon"]].mean()
    return markets.join(xy, on="market_ids")


def shock(markets: pd.DataFrame, runs: pd.DataFrame, normal: pd.DataFrame) -> np.ndarray:
    """z for each row of markets; missing before the runs start or without a centroid."""
    w = 1 / distances(markets["clat"], markets["clon"])
    q = runs.reindex(markets["trimestre"]).to_numpy()
    qbar = normal.reindex(markets["trimestre"]).to_numpy()
    return (q * w).sum(axis=1) / (qbar * w).sum(axis=1) - 1


def main(argv=None) -> None:
    args = parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(message)s", datefmt="%H:%M:%S")

    interim = dd.interim_dir(dd.data_root())
    sample = args.input or interim / f"estimation_sample_{args.system}.csv"
    out = args.out or interim / f"refinery_shock_{args.system}.csv"

    runs = crude_runs(interim / "covariables_mercado" / "covar_sesco_refinacion.csv")
    normal = normal_runs(runs)
    log.info("crude runs %s to %s, %.1f million m3 a year at the eight refineries",
             runs.index[0], runs.index[-1], runs.sum(axis=1).mean() * 4 / 1e6)

    markets = pd.read_csv(sample, encoding="utf-8-sig",
                          usecols=["market_ids", "provincia", "departamento", "trimestre"])
    markets = markets.drop_duplicates("market_ids").reset_index(drop=True)
    centroids = pd.read_csv(interim / "centroides_departamento.csv", encoding="utf-8-sig")
    markets = market_centroids(markets, centroids)
    no_centroid = markets["clat"].isna()
    if no_centroid.any():
        log.warning("%d markets in %d departments have no centroid and get no shock",
                    no_centroid.sum(), markets.loc[no_centroid, "departamento"].nunique())

    markets["refinery_shock"] = shock(markets, runs, normal)
    z = markets.dropna(subset=["refinery_shock"])
    # Quarter effects take out the national part of the shock; what the
    # estimation can use is what is left within the quarter.
    within = z["refinery_shock"] - z.groupby("trimestre")["refinery_shock"].transform("mean")
    log.info("%d of %d markets have a shock, %s to %s; sd %.3f, %.0f%% of the variance within quarter",
             len(z), len(markets), z["trimestre"].min(), z["trimestre"].max(),
             z["refinery_shock"].std(), 100 * within.var() / z["refinery_shock"].var())

    markets[["market_ids", "refinery_shock"]].to_csv(out, index=False, encoding="utf-8-sig")
    log.info("written to %s", out)


if __name__ == "__main__":
    main()

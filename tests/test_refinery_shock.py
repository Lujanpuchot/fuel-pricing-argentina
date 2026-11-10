"""Tests for code/refinery_shock.py, on made-up runs.

    python -m unittest discover tests
"""

import sys
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "code"))
import refinery_shock as rs  # noqa: E402

REFS = list(rs.REFINERIES)


def runs(values):
    """A runs table with every refinery at 100 except where values says."""
    quarters = [f"{y}T{k}" for y in (2010, 2011, 2012) for k in (1, 2, 3, 4)]
    q = pd.DataFrame(100.0, index=pd.Index(quarters, name="trimestre"), columns=REFS)
    for (quarter, ref), v in values.items():
        q.loc[quarter, ref] = v
    return q


class TestShock(unittest.TestCase):

    def test_normal_is_same_quarter_of_adjacent_years(self):
        q = runs({("2010T2", "La Plata"): 80.0, ("2012T2", "La Plata"): 120.0})
        normal = rs.normal_runs(q)
        self.assertEqual(normal.loc["2011T2", "La Plata"], 100.0)  # (80 + 120) / 2
        self.assertEqual(normal.loc["2010T2", "La Plata"], 100.0)  # 2011 only
        self.assertEqual(normal.loc["2012T2", "La Plata"], 100.0)
        self.assertEqual(normal.loc["2011T3", "La Plata"], 100.0)

    def test_zero_in_a_normal_quarter(self):
        q = runs({})
        m = pd.DataFrame({"trimestre": ["2011T1"], "clat": [-34.6], "clon": [-58.4]})
        self.assertAlmostEqual(rs.shock(m, q, rs.normal_runs(q))[0], 0.0)

    def test_nearer_refinery_weighs_more(self):
        # La Plata stops for a quarter: the shock is much larger next to it
        # than in Salta, and Campo Durán, which is next to Salta, still counts
        q = runs({("2011T2", "La Plata"): 0.0})
        m = pd.DataFrame({"trimestre": ["2011T2", "2011T2"],
                          "clat": [-34.92, -24.79], "clon": [-57.95, -65.41]})
        z = rs.shock(m, q, rs.normal_runs(q))
        self.assertLess(z[0], -0.5)
        self.assertLess(z[0], z[1])
        self.assertLess(z[1], 0.0)

    def test_missing_before_runs_start(self):
        q = runs({})
        m = pd.DataFrame({"trimestre": ["2009T4"], "clat": [-34.6], "clon": [-58.4]})
        self.assertTrue(np.isnan(rs.shock(m, q, rs.normal_runs(q))[0]))

    def test_merged_departments_take_the_mean_centroid(self):
        markets = pd.DataFrame({"market_ids": ["6217|2015T1", "X|2015T1"],
                                "provincia": ["BUENOS AIRES", "CAPITAL FEDERAL"],
                                "departamento": ["Chascomús + Lezama", "CABA"],
                                "trimestre": ["2015T1", "2015T1"]})
        centroids = pd.DataFrame({"provincia": ["BUENOS AIRES", "BUENOS AIRES", "CAPITAL FEDERAL"],
                                  "departamento": ["Chascomús", "Lezama", "CABA"],
                                  "clat": [-35.6, -35.8, np.nan], "clon": [-58.0, -57.8, np.nan]})
        out = rs.market_centroids(markets, centroids)
        self.assertAlmostEqual(out["clat"].iloc[0], -35.7)
        self.assertAlmostEqual(out["clon"].iloc[0], -57.9)
        self.assertEqual(tuple(out[["clat", "clon"]].iloc[1]), rs.CITY_CENTROID[("CAPITAL FEDERAL", "CABA")])


if __name__ == "__main__":
    unittest.main()

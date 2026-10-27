"""Tests for code/demand_data.py. They need numpy and pandas, not PyBLP or the data.

    python -m unittest discover tests
"""

import sys
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "code"))
import demand_data as dd  # noqa: E402


def toy_sample():
    """Three markets in two quarters, two grades."""
    return pd.DataFrame({
        "market_ids": ["a|2024T3"] * 3 + ["b|2024T3"] * 2 + ["c|2024T4"] * 2,
        "product_ids": ["YPF|super", "YPF|premium", "SHELL|super",
                        "YPF|super", "SHELL|super", "YPF|super", "YPF|premium"],
        "firm_ids": ["YPF", "YPF", "SHELL", "YPF", "SHELL", "YPF", "YPF"],
        "grade": ["super", "premium", "super", "super", "super", "super", "premium"],
        "prices": [1000.0, 1200.0, 1010.0, 990.0, 1005.0, 1100.0, 1300.0],
        "shares": [0.3, 0.1, 0.2, 0.4, 0.3, 0.5, 0.2],
        "outlets": [3, 3, 2, 1, 1, 4, 4],
        "provincia": ["X"] * 7,
        "trimestre": ["2024T3"] * 5 + ["2024T4"] * 2,
    })


class TestSample(unittest.TestCase):

    def test_deflate_to_base_quarter(self):
        cpi = pd.Series({"2024T3": 100.0, "2024T4": 110.0})
        out = dd.deflate(toy_sample(), cpi, base="2024T4", scale=1000)
        # base quarter: only the change of units
        self.assertAlmostEqual(out["prices"].iloc[5], 1.1)
        # earlier quarter: carried forward by the CPI
        self.assertAlmostEqual(out["prices"].iloc[0], 1000 * 1.1 / 1000)
        self.assertEqual(out["price_nominal"].iloc[0], 1000.0)

    def test_deflate_missing_quarter(self):
        with self.assertRaises(ValueError):
            dd.deflate(toy_sample(), pd.Series({"2024T4": 110.0}), base="2024T4")

    def test_screen_drops_whole_market(self):
        d = toy_sample()
        d.loc[3, "prices"] = 99000.0  # one wrong price in market b
        kept, report = dd.screen_prices(d, 0.5, 2.0)
        self.assertNotIn("b|2024T3", set(kept["market_ids"]))
        self.assertEqual(report["markets_dropped"], 1)
        self.assertEqual(len(kept), 5)

    def test_complete_markets(self):
        d = toy_sample()
        d["d_rival_min"] = [1.0, 1.0, np.nan, 2.0, 2.0, 3.0, 3.0]
        kept, lost = dd.complete_markets(d, ["d_rival_min"])
        self.assertEqual(lost, 1)
        self.assertEqual(set(kept["market_ids"]), {"b|2024T3", "c|2024T4"})

    def test_sample_markets_keeps_every_product(self):
        d = toy_sample()
        s = dd.sample_markets(d, 2, seed=3)
        self.assertEqual(s["market_ids"].nunique(), 2)
        sizes = d.groupby("market_ids").size()
        for m, n in s.groupby("market_ids").size().items():
            self.assertEqual(n, sizes[m])
        pd.testing.assert_frame_equal(s, dd.sample_markets(d, 2, seed=3))

    def test_check_shares(self):
        d = toy_sample()
        dd.check_shares(d)
        d.loc[6, "shares"] = 0.6  # market c now sums to 1.1
        with self.assertRaises(ValueError):
            dd.check_shares(d)


class TestFirstStage(unittest.TestCase):

    def test_demean_matches_dummies(self):
        rng = np.random.default_rng(0)
        n = 400
        g1 = rng.integers(0, 20, n)
        g2 = rng.integers(0, 8, n)
        x = rng.normal(size=n) + 0.5 * g1 - 0.3 * g2
        # residual from OLS on both sets of dummies
        dummies = np.column_stack([g1 == k for k in range(20)] + [g2 == k for k in range(1, 8)])
        coef = np.linalg.lstsq(dummies.astype(float), x, rcond=None)[0]
        expected = x - dummies @ coef
        got = dd.demean(x, [g1, g2]).ravel()
        np.testing.assert_allclose(got, expected, atol=1e-7)

    def simulate(self, strength, n=3000, seed=1):
        rng = np.random.default_rng(seed)
        z = rng.normal(size=(n, 2))
        product = rng.integers(0, 30, n)
        quarter = rng.integers(0, 12, n)
        price = 0.1 * product + strength * z[:, 0] + rng.normal(size=n)
        return pd.DataFrame({
            "prices": price, "outlets": rng.poisson(3, n).astype(float),
            "z1": z[:, 0], "z2": z[:, 1],
            "z_fixed": product.astype(float),  # absorbed by the product effects
            "product_ids": product, "trimestre": quarter,
            "clustering_ids": rng.integers(0, 200, n),
        })

    def test_strong_and_weak_instruments(self):
        args = dict(endog="prices", exog=["outlets"], absorb=["product_ids", "trimestre"],
                    cluster="clustering_ids")
        strong = dd.first_stage(self.simulate(1.0), excluded=["z1", "z2"], **args)
        weak = dd.first_stage(self.simulate(0.0), excluded=["z1", "z2"], **args)
        self.assertGreater(strong["effective_f"], 100)
        self.assertLess(weak["effective_f"], 10)
        # price was simulated as z1 + noise, so the coefficient is near one
        self.assertAlmostEqual(strong["coefficients"]["z1"], 1.0, delta=0.1)
        self.assertLess(abs(strong["coefficients"]["z2"]), 3 * strong["standard_errors"]["z2"])

    def test_absorbed_instrument_is_dropped(self):
        out = dd.first_stage(self.simulate(1.0), "prices", ["outlets"], ["z_fixed", "z1"],
                             ["product_ids", "trimestre"], "clustering_ids")
        self.assertEqual(out["instruments_dropped"], ["z_fixed"])
        self.assertEqual(out["instruments_used"], 1)
        self.assertEqual(list(out["coefficients"]), ["z1"])
        self.assertAlmostEqual(out["coefficients"]["z1"], 1.0, delta=0.1)

    def test_quarter_and_repeated_instruments_are_dropped(self):
        d = self.simulate(1.0)
        d = d[~((d["product_ids"] < 10) & (d["trimestre"] < 4))]  # unbalanced panel
        # a national series, absorbed only by the second set of fixed effects
        d["z_quarter"] = 50 + 10 * np.sin(d["trimestre"].to_numpy(float))
        d["z1_again"] = 2 * d["z1"]
        out = dd.first_stage(d, "prices", ["outlets"], ["z1", "z_quarter", "z1_again", "z2"],
                             ["product_ids", "trimestre"], "clustering_ids")
        self.assertEqual(out["instruments_dropped"], ["z_quarter", "z1_again"])
        self.assertEqual(list(out["coefficients"]), ["z1", "z2"])


if __name__ == "__main__":
    unittest.main()

# ANCHOR: all
# The test of step 1: python3 -I -m unittest discover -s answers/runtime
# ステップ1のテスト
import os
import sys
import unittest

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, here)
sys.path.insert(0, os.path.join(here, "..", "..", "runtime"))
import lam  # noqa: E402
import answers  # noqa: E402

lam.PRIMS.update(answers.MORE_PRIMS)


class TimeFormat(unittest.TestCase):
    # 「UTC の 23:30 は、+09:00 のゾーンでは次の日の 08:30」
    def test_format(self):
        t = lam.lam_time(1_795_995_000_000)
        tokyo = lam.Con("Zone", 0, (540,))
        utc = lam.Con("Zone", 0, (0,))
        fmt = lam.prim("Time.format")
        self.assertEqual(fmt(utc)(t), "2026-11-29 23:30:00 +00:00")
        self.assertEqual(fmt(tokyo)(t), "2026-11-30 08:30:00 +09:00")
        self.assertEqual(fmt(lam.Con("Zone", 0, (-150,)))(t),
                         "2026-11-29 21:00:00 -02:30")


if __name__ == "__main__":
    unittest.main()
# ANCHOR_END: all

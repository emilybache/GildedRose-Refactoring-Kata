# -*- coding: utf-8 -*-
import sys
import unittest
from pathlib import Path

_EXERCISE_ROOT = Path(__file__).resolve().parent.parent
if str(_EXERCISE_ROOT) not in sys.path:
    sys.path.insert(0, str(_EXERCISE_ROOT))

from gilded_rose import Item, GildedRose, NormalStrategy


class GildedRoseTest(unittest.TestCase):
    def test_normal_item_at_zero_quality_stays_zero(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)

        
class TestNormalStrategy(unittest.TestCase):
    """Tests for NormalStrategy — default degradation behaviour."""

    def setUp(self):
        self.strategy = NormalStrategy()

    def test_quality_decrements_by_one_each_day(self):
        item = Item("widget", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(19, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_floors_at_zero(self):
        # Quality must never go negative
        item = Item("widget", sell_in=5, quality=0)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)

    def test_quality_degrades_twice_after_sell_date(self):
        # Once sell_in goes below 0, each day removes 2 quality
        item = Item("widget", sell_in=0, quality=10)
        self.strategy.update(item, days=1)
        self.assertEqual(8, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_multi_day_crosses_sell_date(self):
        # Days=5 from sell_in=2, quality=10
        # Day1: q=9 si=1 | Day2: q=8 si=0 | Day3: q=6 si=-1
        # Day4: q=4 si=-2 | Day5: q=2 si=-3
        item = Item("widget", sell_in=2, quality=10)
        self.strategy.update(item, days=5)
        self.assertEqual(2, item.quality)
        self.assertEqual(-3, item.sell_in)

    def test_quality_floors_at_zero_when_crossing_sell_date(self):
        # sell_in=1, quality=1: Day1 q=0 si=0 | Day2 post-sell but already 0
        item = Item("widget", sell_in=1, quality=1)
        self.strategy.update(item, days=2)
        self.assertEqual(0, item.quality)


if __name__ == '__main__':
    unittest.main()

# -*- coding: utf-8 -*-
import sys
import unittest
from pathlib import Path

_EXERCISE_ROOT = Path(__file__).resolve().parent.parent
if str(_EXERCISE_ROOT) not in sys.path:
    sys.path.insert(0, str(_EXERCISE_ROOT))

from gilded_rose import (
    Item, GildedRose, NormalStrategy, AgedBrieStrategy,
    BackstagePassStrategy, SulfurasStrategy,
)


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


class TestAgedBrieStrategy(unittest.TestCase):
    """Tests for AgedBrieStrategy — quality improves with age."""

    def setUp(self):
        self.strategy = AgedBrieStrategy()

    def test_quality_increments_by_one_each_day(self):
        item = Item("Aged Brie", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(21, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_caps_at_50(self):
        item = Item("Aged Brie", sell_in=10, quality=50)
        self.strategy.update(item, days=1)
        self.assertEqual(50, item.quality)

    def test_quality_increments_twice_after_sell_date(self):
        # Past sell date: +2 per day
        item = Item("Aged Brie", sell_in=0, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(22, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_multi_day_crosses_sell_date(self):
        # sell_in=1, quality=46, days=3
        # Day1: q=47 si=0 | Day2: q=49 si=-1 | Day3: q=50 (capped) si=-2
        item = Item("Aged Brie", sell_in=1, quality=46)
        self.strategy.update(item, days=3)
        self.assertEqual(50, item.quality)
        self.assertEqual(-2, item.sell_in)


class TestBackstagePassStrategy(unittest.TestCase):
    """Tests for BackstagePassStrategy — tiered increase, zeroes after concert."""

    def setUp(self):
        self.strategy = BackstagePassStrategy()

    def test_quality_increments_by_1_when_more_than_10_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(21, item.quality)
        self.assertEqual(14, item.sell_in)

    def test_quality_increments_by_2_when_10_or_fewer_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(22, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_increments_by_3_when_5_or_fewer_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(23, item.quality)
        self.assertEqual(4, item.sell_in)

    def test_quality_drops_to_zero_on_concert_day(self):
        # sell_in=0 means the concert is today — quality zeroes out
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=30)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_quality_stays_zero_after_concert(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=-1, quality=0)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)

    def test_quality_caps_at_50(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)
        self.strategy.update(item, days=1)
        self.assertEqual(50, item.quality)

    def test_multi_day_crosses_all_thresholds(self):
        # sell_in=12, quality=20, days=4
        # Day1(si=12): +1 q=21 si=11 | Day2(si=11): +1 q=22 si=10
        # Day3(si=10): +2 q=24 si=9  | Day4(si=9):  +2 q=26 si=8
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=12, quality=20)
        self.strategy.update(item, days=4)
        self.assertEqual(26, item.quality)
        self.assertEqual(8, item.sell_in)


class TestSulfurasStrategy(unittest.TestCase):
    """Tests for SulfurasStrategy — legendary item, never changes."""

    def setUp(self):
        self.strategy = SulfurasStrategy()

    def test_quality_never_changes(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=1)
        self.assertEqual(80, item.quality)

    def test_sell_in_never_changes(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.sell_in)

    def test_unchanged_after_many_days(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=100)
        self.assertEqual(80, item.quality)
        self.assertEqual(0, item.sell_in)


if __name__ == '__main__':
    unittest.main()

# -*- coding: utf-8 -*-
"""Characterization tests pinning the existing Gilded Rose behaviour.

Written BEFORE refactoring so we can change the implementation freely while
guaranteeing observable behaviour stays identical. Conjured coverage is added
in a later commit once the new rule is implemented.
"""
import unittest

from gilded_rose import Item, GildedRose

BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert"
SULFURAS = "Sulfuras, Hand of Ragnaros"


def update(name, sell_in, quality):
    item = Item(name, sell_in, quality)
    GildedRose([item]).update_quality()
    return item


class NormalItemTest(unittest.TestCase):
    def test_quality_and_sell_in_drop_by_one(self):
        item = update("foo", 10, 20)
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 19)

    def test_quality_drops_twice_as_fast_after_sell_by(self):
        item = update("foo", 0, 20)
        self.assertEqual(item.quality, 18)

    def test_quality_never_negative(self):
        item = update("foo", 5, 0)
        self.assertEqual(item.quality, 0)


class AgedBrieTest(unittest.TestCase):
    def test_increases_in_quality(self):
        item = update("Aged Brie", 5, 20)
        self.assertEqual(item.quality, 21)

    def test_increases_twice_as_fast_after_sell_by(self):
        item = update("Aged Brie", -1, 20)
        self.assertEqual(item.quality, 22)

    def test_quality_capped_at_50(self):
        item = update("Aged Brie", 5, 50)
        self.assertEqual(item.quality, 50)


class SulfurasTest(unittest.TestCase):
    def test_never_changes(self):
        item = update(SULFURAS, 0, 80)
        self.assertEqual(item.sell_in, 0)
        self.assertEqual(item.quality, 80)


class BackstagePassTest(unittest.TestCase):
    def test_increases_by_one_when_far_out(self):
        item = update(BACKSTAGE, 15, 20)
        self.assertEqual(item.quality, 21)

    def test_increases_by_two_within_ten_days(self):
        item = update(BACKSTAGE, 10, 20)
        self.assertEqual(item.quality, 22)

    def test_increases_by_three_within_five_days(self):
        item = update(BACKSTAGE, 5, 20)
        self.assertEqual(item.quality, 23)

    def test_drops_to_zero_after_concert(self):
        item = update(BACKSTAGE, 0, 20)
        self.assertEqual(item.quality, 0)

    def test_quality_capped_at_50(self):
        item = update(BACKSTAGE, 5, 49)
        self.assertEqual(item.quality, 50)
class ConjuredItemTest(unittest.TestCase):
    def test_degrades_twice_as_fast_as_normal(self):
        item = update("Conjured Mana Cake", 5, 20)
        self.assertEqual(item.quality, 18)

    def test_degrades_four_per_day_after_sell_by(self):
        item = update("Conjured Mana Cake", 0, 20)
        self.assertEqual(item.quality, 16)

    def test_quality_never_negative(self):
        item = update("Conjured Mana Cake", 5, 1)
        self.assertEqual(item.quality, 0)

if __name__ == "__main__":
    unittest.main()
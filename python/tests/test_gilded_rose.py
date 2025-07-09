# -*- coding: utf-8 -*-
import unittest
import os
import sys
from models import Item

# insérer le dossier parent dans sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from gilded_rose import GildedRose




# class GildedRoseTest(unittest.TestCase):
#     def test_foo(self):
#         items = [Item("foo", 0, 0)]
#         gilded_rose = GildedRose(items)
#         gilded_rose.update_quality()
#         self.assertEqual("fixme", items[0].name)

class TestNormalItem(unittest.TestCase):
    def test_quality_decreases_by_one_before_expiration(self):
        items = [Item(name="+5 Dexterity Vest", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 19)
        self.assertEqual(items[0].sell_in, 9)

    def test_quality_decreases_by_two_after_expiration(self):
        items = [Item(name="Elixir of the Mongoose", sell_in=0, quality=6)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 4)
        self.assertEqual(items[0].sell_in, -1)

    def test_quality_never_negative(self):
        items = [Item(name="Elixir of the Mongoose", sell_in=0, quality=0)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 0)


class TestAgedBrie(unittest.TestCase):
    def test_quality_increases_by_one_before_expiration(self):
        items = [Item(name="Aged Brie", sell_in=2, quality=0)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 1)
        self.assertEqual(items[0].sell_in, 1)

    def test_quality_increases_by_two_after_expiration(self):
        items = [Item(name="Aged Brie", sell_in=0, quality=0)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 2)
        self.assertEqual(items[0].sell_in, -1)

    def test_quality_never_exceeds_fifty(self):
        items = [Item(name="Aged Brie", sell_in=5, quality=50)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 50)


class TestSulfuras(unittest.TestCase):
    def test_quality_and_sellin_never_change(self):
        items = [Item(name="Sulfuras, Hand of Ragnaros", sell_in=5, quality=80)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 80)
        self.assertEqual(items[0].sell_in, 5)


class TestBackstagePasses(unittest.TestCase):
    def test_quality_increases_by_one_above_10_days(self):
        items = [Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 21)

    def test_quality_increases_by_two_between_10_and_6_days(self):
        items = [Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 22)

    def test_quality_increases_by_three_between_5_and_1_days(self):
        items = [Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 23)

    def test_quality_drops_to_zero_after_concert(self):
        items = [Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 0)

    def test_quality_never_exceeds_fifty(self):
        items = [Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 50)


class TestConjuredItems(unittest.TestCase):
    def test_quality_degrades_twice_as_fast_before_expiration(self):
        items = [Item(name="Conjured Mana Cake", sell_in=3, quality=6)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 4)

    def test_quality_degrades_twice_as_fast_after_expiration(self):
        items = [Item(name="Conjured Mana Cake", sell_in=0, quality=6)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 2)

    def test_quality_never_negative(self):
        items = [Item(name="Conjured Mana Cake", sell_in=0, quality=1)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 0)

        
if __name__ == '__main__':
    unittest.main()

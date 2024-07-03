# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("foo", items[0].name)

    def test_constant_names(self):
        items = [Item("meow", 0, 0), Item("bark", 0, 0), Item("roar", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("meow", items[0].name)
        self.assertEqual("bark", items[1].name)
        self.assertEqual("roar", items[2].name)

    def test_day_passes(self):  # (sellin decreases)
        items = [Item("meow", 1, 10), Item("bark", 2, 10), Item("roar", 3, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(1, items[1].sell_in)
        self.assertEqual(2, items[2].sell_in)

    def test_quality_decreases(self):   # Not for Aged Brie
        items = [Item("meow", 10, 1), Item("bark", 10, 2), Item("roar", 10, 3)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(1, items[1].quality)
        self.assertEqual(2, items[2].quality)

    def test_aged_brief(self):
        items = [Item("Aged Brie", 10, 0), Item("Aged Brie", -1, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(1, items[0].quality)
        self.assertEqual(2, items[1].quality)

    def test_passed_sellin(self):
        items = [Item("banana", 0, 10), Item("apple", -1, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(8, items[0].quality)
        self.assertEqual(8, items[1].quality)

    def test_quality_always_positive(self):
        items = [Item("banana", -1, 0), Item("apple", 1, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(0, items[1].quality)

    def test_quality_not_more_50(self):
        items = [Item("Aged Brie", 10, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)

    def test_sulfuras_never_decrease(self):
        items = [Item("Sulfuras, Hand of Ragnaros", -1, 80), Item("Sulfuras, Hand of Ragnaros", 0, 80), Item("Sulfuras, Hand of Ragnaros", 1, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        # self.assertEqual(80, items[0].quality)
        self.assertEqual(80, items[1].quality)
        self.assertEqual(80, items[2].quality)

    def test_backstage_passes(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 10), Item("Backstage passes to a Coldplay concert", 9, 10),
                 Item("Backstage passes", 5, 10), Item("Backstage passes", 4, 10),
                 Item("Backstage passes", 0, 10), Item("Backstage passes", -1, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(12, items[0].quality)
        self.assertEqual(12, items[1].quality)
        self.assertEqual(13, items[2].quality)
        self.assertEqual(13, items[3].quality)
        self.assertEqual(0, items[4].quality)
        self.assertEqual(0, items[5].quality)

    def test_conjured(self):
        items = [Item("Conjured Cake", -1, 10), Item("Conjured Cookie", 1, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(8, items[0].quality)
        self.assertEqual(8, items[1].quality)

if __name__ == '__main__':
    unittest.main()

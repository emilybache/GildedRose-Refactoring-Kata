# -*- coding: utf-8 -*-
import unittest
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__) + '/../'))

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        # The name should remain unchanged, but quality and sell_in should decrease
        self.assertEqual("foo", items[0].name)
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

        
class GildedRoseFullCoverageTest(unittest.TestCase):
    def test_aged_brie_increases_quality(self):
        items = [Item("Aged Brie", 2, 0)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(1, items[0].quality)
        self.assertEqual(1, items[0].sell_in)

    def test_aged_brie_max_quality(self):
        items = [Item("Aged Brie", 2, 50)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(50, items[0].quality)

    def test_sulfuras_never_changes(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(80, items[0].quality)
        self.assertEqual(0, items[0].sell_in)

    def test_backstage_passes_increase(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(21, items[0].quality)
        items[0].sell_in = 10
        items[0].quality = 20
        gr.update_quality()
        self.assertEqual(22, items[0].quality)
        items[0].sell_in = 5
        items[0].quality = 20
        gr.update_quality()
        self.assertEqual(23, items[0].quality)

    def test_backstage_passes_zero_after_concert(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(0, items[0].quality)

    def test_conjured_degrades_twice(self):
        items = [Item("Conjured Mana Cake", 3, 6)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(4, items[0].quality)
        items[0].sell_in = 0
        items[0].quality = 6
        gr.update_quality()
        self.assertEqual(2, items[0].quality)

    def test_quality_never_negative(self):
        items = [Item("foo", 0, 0)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertGreaterEqual(items[0].quality, 0)

    def test_quality_never_above_50(self):
        items = [Item("Aged Brie", 2, 50)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertLessEqual(items[0].quality, 50)

    def test_normal_item_degrades(self):
        items = [Item("Normal Item", 1, 10)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(9, items[0].quality)
        self.assertEqual(0, items[0].sell_in)
        gr.update_quality()
        self.assertEqual(7, items[0].quality)


if __name__ == '__main__':
    unittest.main()

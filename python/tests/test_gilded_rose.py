# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_normal_item_degrades(self):
        items = [Item("foo", 10, 10)]
        GildedRose(items).update_quality()
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(9, items[0].quality)

    def test_quality_never_negative(self):
        items = [Item("foo", 0, 0)]
        GildedRose(items).update_quality()
        self.assertEqual(0, items[0].quality)

    def test_aged_brie_increases_up_to_50(self):
        items = [Item("Aged Brie", 2, 49)]
        GildedRose(items).update_quality()
        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(50, items[0].quality)

    def test_sulfuras_invariant(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        GildedRose(items).update_quality()
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(80, items[0].quality)

    def test_backstage_passes_increase_and_drop(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 11, 10)]
        GildedRose(items).update_quality()  # 11->10, +1
        self.assertEqual(10, items[0].sell_in)
        self.assertEqual(11, items[0].quality)

        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 10)]
        GildedRose(items).update_quality()  # +2
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(12, items[0].quality)

        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 10)]
        GildedRose(items).update_quality()  # +3
        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(13, items[0].quality)

        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 10)]
        GildedRose(items).update_quality()  # -> 0
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

    def test_conjured_items_degrade_twice_as_fast(self):
        items = [Item("Conjured Mana Cake", 3, 6)]
        GildedRose(items).update_quality()
        self.assertEqual(2, items[0].sell_in)
        self.assertEqual(4, items[0].quality)

    def test_conjured_items_degrade_four_after_sell_date(self):
        items = [Item("Conjured Something", 0, 6)]
        GildedRose(items).update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(2, items[0].quality)

        
if __name__ == '__main__':
    unittest.main()

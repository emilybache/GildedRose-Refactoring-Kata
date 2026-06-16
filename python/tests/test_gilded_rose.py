# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_normal_item_before_sell_in(self):
        items = [Item("Normal", 5, 10)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 9)
        self.assertEqual(items[0].sell_in, 4)

    def test_normal_item_after_sell_in(self):
        items = [Item("Normal", 0, 10)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 8)
        self.assertEqual(items[0].sell_in, -1)

    def test_aged_brie_increases_quality(self):
        items = [Item("Aged Brie", 2, 0)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 1)
        self.assertEqual(items[0].sell_in, 1)

    def test_aged_brie_never_exceeds_50(self):
        items = [Item("Aged Brie", 2, 50)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 50)
        self.assertEqual(items[0].sell_in, 1)

    def test_backstage_passes_increases_by_1_when_sell_in_more_than_10(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 21)
        self.assertEqual(items[0].sell_in, 14)

    def test_backstage_passes_increases_by_2_when_sell_in_10_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 22)
        self.assertEqual(items[0].sell_in, 9)

    def test_backstage_passes_increases_by_3_when_sell_in_5_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 23)
        self.assertEqual(items[0].sell_in, 4)

    def test_backstage_passes_quality_drops_to_zero_after_concert(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 0)
        self.assertEqual(items[0].sell_in, -1)

    def test_sulfuras_never_changes(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 80)
        self.assertEqual(items[0].sell_in, 0)

    def test_sulfuras_with_positive_sell_in_also_unchanged(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 5, 80)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 80)
        self.assertEqual(items[0].sell_in, 5)

    def test_quality_never_negative(self):
        items = [Item("Normal", 0, 0)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 0)

    def test_conjured_items_are_not_supported_yet_but_work_as_normal(self):
        items = [Item("Conjured Mana Cake", 3, 6)]
        gilded = GildedRose(items)
        gilded.update_quality()
        self.assertEqual(items[0].quality, 5)
        self.assertEqual(items[0].sell_in, 2)

        
if __name__ == '__main__':
    unittest.main()

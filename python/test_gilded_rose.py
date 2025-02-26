# -*- coding: utf-8 -*-
import unittest
from gilded_rose import Item, GildedRose

class GildedRoseTest(unittest.TestCase):
    def test_regular_item(self):
        items = [Item("+5 Dexterity Vest", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, 9)
        self.assertEqual(items[0].quality, 19)

    def test_aged_brie_increases_in_quality(self):
        items = [Item("Aged Brie", sell_in=2, quality=0)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, 1)
        self.assertEqual(items[0].quality, 1)

    def test_sulfuras_does_not_decrease(self):
        items = [Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, 0)
        self.assertEqual(items[0].quality, 80)

    def test_backstage_passes_increase_in_quality(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, 9)
        self.assertEqual(items[0].quality, 22)  # Increases by 2 when sell_in <= 10

    def test_backstage_passes_drop_to_zero_after_concert(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=30)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, -1)
        self.assertEqual(items[0].quality, 0)

    def test_quality_never_exceeds_50(self):
        items = [Item("Aged Brie", sell_in=5, quality=50)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 50)

    def test_quality_never_goes_below_zero(self):
        items = [Item("+5 Dexterity Vest", sell_in=5, quality=0)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].quality, 0)

    def test_expired_item_degrades_twice_as_fast(self):
        items = [Item("+5 Dexterity Vest", sell_in=0, quality=10)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, -1)
        self.assertEqual(items[0].quality, 8)  # Decreases by 2 after expiry

    def test_conjured_item_degrades_twice_as_fast(self):
        items = [Item("Conjured Mana Cake", sell_in=3, quality=6)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, 2)
        self.assertEqual(items[0].quality, 5)

    def test_conjured_item_degrades_four_times_as_fast_when_expired(self):
        items = [Item("Conjured Mana Cake", sell_in=0, quality=6)]
        GildedRose(items).update_quality()
        self.assertEqual(items[0].sell_in, -1)
        self.assertEqual(items[0].quality, 4)

if __name__ == '__main__':
    unittest.main()

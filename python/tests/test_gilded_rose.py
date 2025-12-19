# -*- coding: utf-8 -*-
import unittest
from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):

    # Normal items

    def test_normal_item_before_sell_date_degrades_by_1(self):
        items = [Item("Normal Item", 10, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(19, items[0].quality)

    def test_normal_item_after_sell_date_degrades_by_2(self):
        items = [Item("Normal Item", 0, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(18, items[0].quality)

    def test_normal_item_quality_never_negative(self):
        items = [Item("Normal Item", 10, 0)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

    # Aged Brie

    def test_aged_brie_increases_by_1_before_sell_date(self):
        items = [Item("Aged Brie", 2, 0)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(1, items[0].quality)

    def test_aged_brie_increases_by_2_after_sell_date(self):
        items = [Item("Aged Brie", 0, 0)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(2, items[0].quality)

    def test_aged_brie_quality_capped_at_50(self):
        items = [Item("Aged Brie", 5, 50)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(50, items[0].quality)

    # Backstage passes

    def test_backstage_passes_increase_by_1_when_sell_in_gt_10(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(14, items[0].sell_in)
        self.assertEqual(21, items[0].quality)

    def test_backstage_passes_increase_by_2_when_sell_in_le_10_gt_5(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(22, items[0].quality)

    def test_backstage_passes_increase_by_3_when_sell_in_le_5_gt_0(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(23, items[0].quality)

    def test_backstage_passes_drop_to_zero_after_concert(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

    def test_backstage_passes_quality_capped_at_50(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(50, items[0].quality)  # 49 + 3 => capped at 50

    # Sulfuras

    def test_sulfuras_never_changes(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(80, items[0].quality)

    #Conjured items (new requirement)

    def test_conjured_item_degrades_by_2_before_sell_date(self):
        items = [Item("Conjured Mana Cake", 5, 10)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(8, items[0].quality)

    def test_conjured_item_degrades_by_4_after_sell_date(self):
        items = [Item("Conjured Mana Cake", 0, 10)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(6, items[0].quality)

    def test_conjured_item_quality_never_negative(self):
        items = [Item("Conjured Mana Cake", 0, 3)]
        gr = GildedRose(items)

        gr.update_quality()

        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)


if __name__ == '__main__':
    unittest.main()
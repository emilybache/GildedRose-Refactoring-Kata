# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals("foo", items[0].name)

    def test_sulfuras_constant_quality_and_sell_in_value_update(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 20, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(80, items[0].quality)
        self.assertEqual(20, items[0].sell_in)

    def test_normal_goods_fresh_quality_and_sell_in_value_update(self):
        items = [Item("Rat Soup", 20, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(9, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_normal_goods_rotten_quality_and_sell_in_value_update(self):
        items = [Item("Rat Soup", 0, 5)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(3, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_normal_goods_non_negative_quality(self):
        items = [Item("Rat Soup light", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_aged_brie_quality_and_sell_in_value_standard_update(self):
        items = [Item("Aged Brie", 20, 30)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(31, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_aged_brie_quality_and_sell_in_value_double_update(self):
        items = [Item("Aged Brie", 0, 30)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(32, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_aged_brie_quality_hit_50(self):
        items = [Item("Aged Brie", -5, 49)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)
        self.assertEqual(-6, items[0].sell_in)

    def test_backstage_passes_quality_and_sell_in_value_standard_update(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 20, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(11, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_backstage_passes_quality_and_sell_in_value_double_update(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 8, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(12, items[0].quality)
        self.assertEqual(7, items[0].sell_in)

    def test_backstage_passes_quality_and_sell_in_value_triple_update(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 4, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(13, items[0].quality)
        self.assertEqual(3, items[0].sell_in)

    def test_backstage_passes_quality_and_sell_in_value_after_concert_update(
        self,
    ):  # noqa: E501
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_conjured_items_quality_and_sell_in_value_standard_update(self):
        items = [Item("Conjured Pizza", 5, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(8, items[0].quality)
        self.assertEqual(4, items[0].sell_in)

    def test_conjured_items_quality_and_sell_in_value_rotten_update(self):
        items = [Item("Conjured Pizza", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(6, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_conjured_items_quality_hit_0(self):
        items = [Item("Conjured Pizza", 0, 1)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)


if __name__ == "__main__":
    unittest.main()

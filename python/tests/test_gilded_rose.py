# -*- coding: utf-8 -*-
import unittest

from gilded_rose import GildedRose, Item


class GildedRoseItemTest(unittest.TestCase):

    def test_item_instance(self):
        item = Item("bread", 10, 20)
        self.assertIsInstance(item, Item)

    def test_item_quality(self):
        items = [Item("bread", 20, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(39, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_item_quality_sell_in_passed(self):
        items = [Item("bread", 0, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(38, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_item_quality_sell_can_be_negative(self):
        items = [Item("bread", 20, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

class GildedRoseAgedBrieTest(unittest.TestCase):

    def test_item_quality(self):
        items = [Item("Aged Brie", 20, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(41, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_item_quality_sell_in_passed(self):
        items = [Item("Aged Brie", 0, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(42, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_item_quality_sell_can_be_over_50(self):
        items = [Item("Aged Brie", 20, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

class GildedRoseSulfurasTest(unittest.TestCase):

    def test_item_quality(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 20, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(40, items[0].quality)
        self.assertEqual(20, items[0].sell_in)

    def test_item_quality_sell_in_passed(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(40, items[0].quality)
        self.assertEqual(0, items[0].sell_in)

class GildedRoseBackstagePassTest(unittest.TestCase):


    def test_item_quality(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 20, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(41, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_item_quality_10_days_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(42, items[0].quality)
        self.assertEqual(9, items[0].sell_in)

    def test_item_quality_5_days_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(43, items[0].quality)
        self.assertEqual(4, items[0].sell_in)

    def test_item_quality_sell_in_passed(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

class GildedRoseConjuredItemTest(unittest.TestCase):

    def test_item_quality(self):
        items = [Item("Conjured", 20, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(38, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

    def test_item_quality_sell_in_passed(self):
        items = [Item("Conjured", 0, 40)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(36, items[0].quality)
        self.assertEqual(-1, items[0].sell_in)

    def test_item_quality_sell_can_be_negative(self):
        items = [Item("Conjured", 20, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        self.assertEqual(19, items[0].sell_in)

if __name__ == '__main__':
    unittest.main()

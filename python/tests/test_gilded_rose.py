# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("fixme", items[0].name)
        
    def test_classic_item(self):
        item = Item("Elixir of the Mongoose", 8, 7)
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 7)
        self.assertEqual(item.quality, 6)
        
    def test_sulfuras(self):
        item = Item("Sulfuras, Hand of Ragnaros", 0, 80)
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 0)
        self.assertEqual(item.quality, 80)
        
    def test_aged_brie(self):
        item = Item("Aged Brie", 1, 2)
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 0)
        self.assertEqual(item.quality, 3)
        
        # Two days after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -1)
        self.assertEqual(item.quality, 5)

    def test_backstage_passes(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 11, 12),
                 Item("Backstage passes to a TAFKAL80ETC concert", 1, 45)]
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(items[0].sell_in, 10)
        self.assertEqual(items[0].quality, 13)
        self.assertEqual(items[1].sell_in, 0)
        self.assertEqual(items[1].quality, 48)
        
        # Two days after
        gilded_rose.update_quality()
        self.assertEqual(items[0].sell_in, 9)
        self.assertEqual(items[0].quality, 15)
        self.assertEqual(items[1].sell_in, -1)
        self.assertEqual(items[1].quality, 0)
        
    def test_conjured_item(self):
        item = Item("Conjured Mana Cake", 1, 24)
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 0)
        self.assertEqual(item.quality, 22)
        
        # Two days after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -1)
        self.assertEqual(item.quality, 18)
        
if __name__ == '__main__':
    unittest.main()

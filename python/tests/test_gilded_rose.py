# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):

    def check_item_values(self, item, name, sell_in, quality):
        self.assertEqual(item.name, name)
        self.assertEqual(item.sell_in, sell_in)
        self.assertEqual(item.quality, quality)

    def test_foo_nonnegative(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("foo", items[0].name)
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

    def test_multiple_items(self):
        items = [Item("foo", 0, 0), Item("bar", 0, 0)]
        
if __name__ == '__main__':
    unittest.main()

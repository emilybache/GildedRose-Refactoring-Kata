# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):

    def check_item_values(self, item, name, sell_in, quality):
        self.assertEqual(item.name, name)
        self.assertEqual(item.sell_in, sell_in)
        self.assertEqual(item.quality, quality)

    def generate_and_update_gilded_rose(self, items, days):
        gilded_rose = GildedRose(items)
        for day in range(days):
            gilded_rose.update_gilded_rose()
        return gilded_rose

    def test_foo_1day(self):
        items= [Item('foo', 1, 1)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.check_item_values(items[0], 'foo', 0, 0)


    def test_foo_nonnegative(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.check_item_values(items[0], "foo", 0, 0)


    def test_multiple_items(self):
        items = [Item("foo", 0, 0), Item("bar", 1, 1), Item("baz", 2, 2)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.check_item_values(items[0], "foo", 0, 0)
        self.check_item_values(items[1], "bar", 0, 0)
        self.check_item_values(items[2], "baz", 1, 1)


        
if __name__ == '__main__':
    unittest.main()

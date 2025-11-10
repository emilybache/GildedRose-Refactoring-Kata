# -*- coding: utf-8 -*-
import unittest
from hashlib import file_digest

from gilded_rose import Item, GildedRose

BACKSTAGE = 'Backstage passes to a TAFKAL80ETC concert'
SULFURAS = 'Sulfuras, Hand of Ragnaros'
AGED_BRIE = 'Aged Brie'

class GildedRoseTest(unittest.TestCase):

    def check_item_values(self, item, name, sell_in, quality):
        self.assertEqual(item.name, name)
        self.assertEqual(item.sell_in, sell_in)
        self.assertEqual(item.quality, quality)

    def update_gilded_rose_days(self, gilded_rose, days):
        for day in range(days):
            gilded_rose.update_quality()

    def generate_and_update_gilded_rose(self, items, days):
        gilded_rose = GildedRose(items)
        self.update_gilded_rose_days(gilded_rose, days)
        return gilded_rose

    def test_foo_1day(self):
        items= [Item('foo', 1, 1)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], 'foo', 0, 0)

    def test_foo_multiple_days(self):
        items= [Item('foo', 2, 3)]
        self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], 'foo', 0, 1)

    def test_quality_nonnegative(self):
        items = [Item("foo", 2, 0)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], "foo", 1, 0)

    def test_after_sellby_date(self):
        items = [Item("foo", 2, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], "foo", 0, 8)
        self.update_gilded_rose_days(gilded_rose, 2)
        self.check_item_values(items[0], "foo", -2, 4)

    def test_aged_brie_4_days(self):
        items = [Item(AGED_BRIE, 2, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], AGED_BRIE, 0, 12)
        self.update_gilded_rose_days(gilded_rose, 2)
        self.check_item_values(items[0], AGED_BRIE, -2, 16)

    def test_aged_brie_max_quality(self):
        items = [Item(AGED_BRIE, 15, 40)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 10)
        self.check_item_values(items[0], AGED_BRIE, 5, 50)
        self.update_gilded_rose_days(gilded_rose, 6)
        self.check_item_values(items[0], AGED_BRIE, -1, 50)

    def test_sulfuras(self):
        items = [Item(SULFURAS, 10, 30)]
        self.generate_and_update_gilded_rose(items, 50)
        self.check_item_values(items[0], SULFURAS, 10, 30)

    def test_backstage_passes_15_to_10_days(self):
        items = [Item(BACKSTAGE, 15, 0)]
        self.generate_and_update_gilded_rose(items, 5)
        self.check_item_values(items[0], BACKSTAGE, 10, 5)

    def test_backstage_passes_10_to_5_days(self):
        items = [Item(BACKSTAGE, 10, 0)]
        self.generate_and_update_gilded_rose(items, 5)
        self.check_item_values(items[0], BACKSTAGE, 5, 10)

    def test_backstage_passes_5_to_0_days(self):
        items = [Item(BACKSTAGE, 5, 0)]
        self.generate_and_update_gilded_rose(items, 5)
        self.check_item_values(items[0], BACKSTAGE, 0, 15)

    def test_backstage_passes_after_concert(self):
        items = [Item(BACKSTAGE, 1, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], BACKSTAGE, 0, 13)
        self.update_gilded_rose_days(gilded_rose, 1)
        self.check_item_values(items[0], BACKSTAGE, -1, 0)

    def test_multiple_items_different_sellin_1day(self):
        items = [Item("foo", 0, 0), Item("bar", 1, 1), Item("baz", 2, 2)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], "foo", -1, 0)
        self.check_item_values(items[1], "bar", 0, 0)
        self.check_item_values(items[2], "baz", 1, 1)

    def test_all_items_15_days(self):
        items = [Item("foo", 10, 10), Item(SULFURAS, 10, 10), Item(BACKSTAGE, 10, 10), Item(AGED_BRIE, 10, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 5)
        self.check_item_values(items[0], "foo", 5, 5)
        self.check_item_values(items[1], SULFURAS, 10, 10)
        self.check_item_values(items[2], BACKSTAGE, 5, 20)
        self.check_item_values(items[3], AGED_BRIE, 5, 15)
        self.update_gilded_rose_days(gilded_rose, 5)
        self.check_item_values(items[0], "foo", 0, 0)
        self.check_item_values(items[1], SULFURAS, 10, 10)
        self.check_item_values(items[2], BACKSTAGE, 0, 35)
        self.check_item_values(items[3], AGED_BRIE, 0, 20)
        self.update_gilded_rose_days(gilded_rose, 5)
        self.check_item_values(items[0], "foo", -5, 0)
        self.check_item_values(items[1], SULFURAS, 10, 10)
        self.check_item_values(items[2], BACKSTAGE, -5, 0)
        self.check_item_values(items[3], AGED_BRIE, -5, 30)


if __name__ == '__main__':
    unittest.main()

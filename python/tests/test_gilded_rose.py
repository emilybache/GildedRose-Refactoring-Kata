# -*- coding: utf-8 -*-
import unittest
from parameterized import parameterized

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

    def test_normal_item_single_day_degradation(self):
        items= [Item('foo', 1, 1)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], 'foo', 0, 0)

    def test_normal_item_multiple_days_degradation(self):
        items= [Item('foo', 2, 3)]
        self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], 'foo', 0, 1)

    def test_nonnegative_quality_after_degradation(self):
        items = [Item('foo', 2, 0)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], 'foo', 1, 0)

    def test_quality_degradation_after_sellby_date(self):
        items = [Item('foo', 2, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], 'foo', 0, 8)
        self.update_gilded_rose_days(gilded_rose, 2)
        self.check_item_values(items[0], 'foo', -2, 4)

    def test_aged_brie_after_4_days(self):
        items = [Item(AGED_BRIE, 2, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 2)
        self.check_item_values(items[0], AGED_BRIE, 0, 12)
        self.update_gilded_rose_days(gilded_rose, 2)
        self.check_item_values(items[0], AGED_BRIE, -2, 16)

    def test_aged_brie_reaching_max_quality(self):
        items = [Item(AGED_BRIE, 15, 40)]
        gilded_rose = self.generate_and_update_gilded_rose(items, 10)
        self.check_item_values(items[0], AGED_BRIE, 5, 50)
        self.update_gilded_rose_days(gilded_rose, 6)
        self.check_item_values(items[0], AGED_BRIE, -1, 50)

    def test_sulfuras_no_degradation_not_sold(self):
        items = [Item(SULFURAS, 10, 80)]
        self.generate_and_update_gilded_rose(items, 50)
        self.check_item_values(items[0], SULFURAS, 10, 80)


    @parameterized.expand([
        ('more_than_10_days_inc_by_1', 15, 0, 10, 5),
        ('10_days_inc_by_2', 10, 0, 5, 10),
        ('5_day_inc_by_3', 5, 0, 0, 15),
        ('after_concert_drops_to_zero', 1, 10, -4, 0)
        ])
    def test_backstage_passes_by_5_days_from(self, _, init_sell_in, init_qual, expected_sell_in, expected_qual):
        items = [Item(BACKSTAGE, init_sell_in, init_qual)]
        self.generate_and_update_gilded_rose(items, 5)
        self.check_item_values(items[0], BACKSTAGE, expected_sell_in, expected_qual)

    def test_multiple_items_single_day_degradation(self):
        items = [Item('foo', 0, 0), Item('bar', 1, 1), Item('baz', 2, 2)]
        self.generate_and_update_gilded_rose(items, 1)
        self.check_item_values(items[0], 'foo', -1, 0)
        self.check_item_values(items[1], 'bar', 0, 0)
        self.check_item_values(items[2], 'baz', 1, 1)

    # @parameterized.expand([
    #     ('normal_item_decrements_until_expired', 0, 5, 5, 0, 0, -5, 0),
    #     ('sulfuras_unchanged', 1, 10, 10, 10, 10, 10, 10),
    #     ('backstage_passes_', 2, 5, 20, 0, 35, -5, 0),
    #     ('aged_brie', 3, 5, 15, 0, 20, -5, 30)
    # ])
    @parameterized.expand([
        ('5 days', 5, 5, 5, 10, 80, 5, 20, 5, 15),
        ('10 days', 10, 0, 0, 10, 80, 0, 35, 0, 20),
        ('15 days', 15, -5, 0, 10, 80, -5, 0, -5, 30)
    ])
    def test_multiple_various_items_from_10_days_after(self, _, days, normal_expected_sell_in, normal_expected_quality, sulfuras_expected_sell_in, sulfuras_expected_quality, backstage_expected_sell_in, backstage_expected_quality, aged_brie_expected_sell_in, aged_brie_expected_quality,):
        items = [Item('foo', 10, 10), Item(SULFURAS, 10, 80), Item(BACKSTAGE, 10, 10), Item(AGED_BRIE, 10, 10)]
        gilded_rose = self.generate_and_update_gilded_rose(items, days)
        self.check_item_values(items[0], 'foo', normal_expected_sell_in, normal_expected_quality)
        self.check_item_values(items[1], SULFURAS, sulfuras_expected_sell_in, sulfuras_expected_quality)
        self.check_item_values(items[2], BACKSTAGE, backstage_expected_sell_in, backstage_expected_quality)
        self.check_item_values(items[3], AGED_BRIE, aged_brie_expected_sell_in, aged_brie_expected_quality)


if __name__ == '__main__':
    unittest.main()

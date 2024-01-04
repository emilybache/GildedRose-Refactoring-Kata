# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals("foo", items[0].name)

    def test_simple_quality(self):
        initial_quality = 5
        items = [Item("foo", 5, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality - 1, items[0].quality)

    def test_simple_sell_in(self):
        initial_sell_in = 5
        items = [Item("foo", initial_sell_in, 5)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_sell_in - 1, items[0].sell_in)

    def test_quality_never_negative_simple_item(self):
        initial_quality = 0
        items = [Item("foo1", 1, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertNotEquals(initial_quality - 1, items[0].quality)
        self.assertEquals(0, items[0].quality)

    def test_quality_never_negative_simple_item_passed_sell_date(self):
        initial_quality = 0
        items = [Item("foo1", -2, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertNotEquals(initial_quality - 1, items[0].quality)
        self.assertEquals(0, items[0].quality)

    def test_quality_degrades_twice_fast_after_passed_sell_date(self):
        initial_quality = 10
        items = [Item("foo1", 0, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality - 2, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality - 4, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality - 6, items[0].quality)

    def test_aged_brie_increases_in_quality_in_time(self):
        initial_quality = 10
        items = [Item("Aged Brie", 5, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 1, items[0].quality)

    def test_aged_brie_never_increases_increases_50_in_quality(self):
        initial_quality = 49
        items = [Item("Aged Brie", 5, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 1, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)

    def test_backstage_passes_as_sell_date_approaches_more_than_10_days(self):
        initial_quality = 10
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 1, items[0].quality)

    def test_backstage_passes_as_sell_date_approaches_less_than_10_days(self):
        initial_quality = 10
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 2, items[0].quality)

    def test_backstage_passes_as_sell_date_approaches_less_than_5_days(self):
        initial_quality = 10
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 3, items[0].quality)

    def test_backstage_passes_as_sell_date_passed(self):
        initial_quality = 10
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(0, items[0].quality)

    def test_backstage_passes_never_increases_increases_50_in_quality(self):
        initial_quality = 44
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality + 3, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)

    def test_sulfras(self):
        initial_quality = 80
        items = [Item("Sulfuras, Hand of Ragnaros", 1, initial_quality)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality, items[0].quality)
        gilded_rose.update_quality()
        self.assertEquals(initial_quality, items[0].quality)

        
if __name__ == '__main__':
    unittest.main()

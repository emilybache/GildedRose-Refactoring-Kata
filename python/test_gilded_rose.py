# -*- coding: utf-8 -*-
import unittest

import constants
from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_degrades_twice_as_fast_after_sell_in_date_has_passed(self):
        items = [Item("foo", 0, 2)]
        GildedRose(items).update_quality()
        self.assertEqual(0, items[0].quality)

    def test_never_must_have_negative_quality(self):
        items = [Item("foo", 0, 0)]
        GildedRose(items).update_quality()
        self.assertEqual(0, items[0].quality)

    def test_increase_aged_brie_quality_when_it_gets_older(self):
        items = [Item(constants.AGED_BRIE, 0, 2)]
        GildedRose(items).update_quality()
        self.assertEqual(4, items[0].quality)

    def test_never_must_have_an_item_with_more_than_50_of_quality(self):
        items = [Item(constants.AGED_BRIE, 0, 50)]
        GildedRose(items).update_quality()
        self.assertEqual(50, items[0].quality)

    def test_never_modify_sulfuras_quality(self):
        items = [Item(constants.SULFURAS, 0, 50)]
        GildedRose(items).update_quality()
        self.assertEqual(50, items[0].quality)

    def test_never_modify_sulfuras_quality_even_is_greater_than_50(self):
        items = [Item(constants.SULFURAS, 0, 80)]
        GildedRose(items).update_quality()
        self.assertEqual(80, items[0].quality)

    def test_must_increase_backstage_passes_quality_when_its_sell_in_approaches(self):
        items = [
            Item(constants.BACKSTAGE, 15, 20),
            Item(constants.BACKSTAGE, 10, 20),
            Item(constants.BACKSTAGE, 5, 20),
        ]
        GildedRose(items).update_quality()
        self.assertEqual(21, items[0].quality)
        self.assertEqual(22, items[1].quality)
        self.assertEqual(23, items[2].quality)

    def test_backstage_quality_must_be_zero_after_concert(self):
        items = [Item(constants.BACKSTAGE, 0, 80)]
        GildedRose(items).update_quality()
        self.assertEqual(0, items[0].quality)

    def test_must_decrease_quality_twice_as_fast_if_item_is_conjured(self):
        items = [Item(constants.CONJURED, 0, 20)]
        GildedRose(items).update_quality()
        self.assertEqual(16, items[0].quality)


if __name__ == "__main__":
    unittest.main()

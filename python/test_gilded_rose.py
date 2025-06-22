# -*- coding: utf-8 -*-
# import unittest

# from gilded_rose import Item, GildedRose


# class GildedRoseTest(unittest.TestCase):
#     def test_foo(self):
#         items = [Item("foo", 0, 0)]
#         gilded_rose = GildedRose(items)
#         gilded_rose.update_quality()
#         self.assertEqual("foo", items[0].name)

        
# if __name__ == '__main__':
#     unittest.main()

import pytest
from gilded_rose import GildedRose
from items import Item


def test_regular_item_degrades():
    items = [Item("foo", 10, 20)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 19
    assert items[0].sell_in == 9


def test_aged_brie_increases():
    items = [Item("Aged Brie", 2, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 1
    assert items[0].sell_in == 1


def test_backstage_passes_increase():
    items = [Item("Backstage passes", 11, 10)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 11


def test_sulfuras_no_change():
    items = [Item("Sulfuras", 0, 80)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 80
    assert items[0].sell_in == 0

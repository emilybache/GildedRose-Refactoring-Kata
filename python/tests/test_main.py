# -*- coding: utf-8 -*-
from gilded_rose import Item, GildedRose

def test_foo():
    items = [Item("foo", 0, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].name == "foo"

# At the end of each day our system lowers both values for every item
def test_item_sell_in_decreases():
    items = [Item("foo", 1, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].sell_in == 0

def test_item_quality_decreases():
    items = [Item("foo", 0, 1)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 0

# Once the sell by date has passed, Quality degrades twice as fast
def test_item_sell_in_passed_quality_degrades_twice():
    items = [Item("foo", 0, 2)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 0

# The Quality of an item is never negative
def test_item_quality_is_never_negative():
    items = [Item("foo", 0, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 0

# The Quality of an item is never more than 50
def test_item_quality_is_never_more_than_50():
    items = [Item("Aged Brie", 0, 50)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 50
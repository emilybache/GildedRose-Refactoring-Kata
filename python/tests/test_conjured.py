import pytest

from gilded_rose import Item, Conjured


def test_conjured_degrades_by_2_before_sell_date():
    item = Item("Conjured Mana Cake", sell_in=3, quality=10)
    item.daily_step()
    assert isinstance(item, Conjured)
    assert item.sell_in == 2
    assert item.quality == 8


def test_conjured_degrades_by_4_after_sell_date():
    item = Item("Conjured Mana Cake", sell_in=0, quality=10)
    item.daily_step()
    assert item.sell_in == -1
    assert item.quality == 6


def test_conjured_never_negative():
    item = Item("Conjured Mana Cake", sell_in=0, quality=3)
    item.daily_step()
    assert item.quality == 0


def test_conjured_sell_in_decrements():
    item = Item("Conjured Mana Cake", sell_in=5, quality=10)
    item.daily_step()
    assert item.sell_in == 4


def test_conjured_mapping_via_item_new():
    item = Item("Conjured Mana Cake", sell_in=5, quality=10)
    assert isinstance(item, Conjured)

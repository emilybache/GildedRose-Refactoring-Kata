from python.gilded_rose import (
    GildedRose,
    Item,
    NormalItem,
    AgedBrieItem,
    SulfurasItem,
    BackstagePassItem,
    ConjuredItem,
)
import pytest


# --- Tests for NormalItem ---
def test_normal_item_quality_decreases_by_1():
    item = NormalItem("Westin", 10, 20)
    item.update_quality()
    assert item.quality == 19
    assert item.sell_in == 9


def test_normal_item_quality_decreases_by_2_after_sell_in():
    item = NormalItem("Westin", 0, 20)
    item.update_quality()
    assert item.quality == 18
    assert item.sell_in == -1


def test_normal_item_quality_never_negative():
    item = NormalItem("Westin", 10, 0)
    item.update_quality()
    assert item.quality == 0
    assert item.sell_in == 9


def test_aged_brie_quality_increases():
    item = AgedBrieItem("Aged Brie", 10, 20)
    item.update_quality()
    assert item.quality == 21
    assert item.sell_in == 9


def test_aged_brie_quality_increases_twice_after_sell_in():
    item = AgedBrieItem("Aged Brie", 0, 20)
    item.update_quality()
    assert item.quality == 22
    assert item.sell_in == -1


def test_aged_brie_quality_never_exceeds_50():
    item = AgedBrieItem("Aged Brie", 10, 49)
    item.update_quality()
    assert item.quality == 50
    item.update_quality()  # One more day
    assert item.quality == 50


def test_sulfuras_quality_and_sell_in_never_change():
    item = SulfurasItem("Sulfuras, Hand of Ragnaros", 0, 80)
    item.update_quality()
    assert item.quality == 80
    assert item.sell_in == 0


def test_backstage_pass_quality_increases_by_1_normally():
    item = BackstagePassItem("Backstage passes", 15, 20)
    item.update_quality()
    assert item.quality == 21
    assert item.sell_in == 14


def test_backstage_pass_quality_increases_by_2_at_10_days_or_less():
    item = BackstagePassItem("Backstage passes", 10, 20)
    item.update_quality()
    assert item.quality == 22
    assert item.sell_in == 9


def test_backstage_pass_quality_increases_by_3_at_5_days_or_less():
    item = BackstagePassItem("Backstage passes", 5, 20)
    item.update_quality()
    assert item.quality == 23
    assert item.sell_in == 4


def test_backstage_pass_quality_drops_to_0_after_concert():
    item = BackstagePassItem("Backstage passes", 0, 20)
    item.update_quality()
    assert item.quality == 0
    assert item.sell_in == -1


def test_backstage_pass_quality_never_exceeds_50():
    item = BackstagePassItem("Backstage passes", 10, 49)
    item.update_quality()
    assert item.quality == 50


# Test cases for Conjured Items
def test_conjured_item_quality_decreases_by_2_normally():
    item = ConjuredItem("Conjured Mana Cake", 10, 20)
    item.update_quality()
    assert item.quality == 18
    assert item.sell_in == 9


def test_conjured_item_quality_decreases_by_4_after_sell_in():
    item = ConjuredItem("Conjured Mana Cake", 0, 20)
    item.update_quality()
    assert item.quality == 16
    assert item.sell_in == -1


def test_conjured_item_quality_never_negative():
    item = ConjuredItem("Conjured Mana Cake", 10, 1)
    item.update_quality()
    assert item.quality == 0
    assert item.sell_in == 9

import pytest

from gilded_rose import Item, Sulfuras


def test_sulfuras_is_immutable_over_time():
    item = Item("Sulfuras, Hand of Ragnaros", sell_in=5, quality=80)
    item.daily_step()
    assert isinstance(item, Sulfuras)
    assert item.sell_in == 0  # forced to 0 in constructor
    assert item.quality == 80


def test_sulfuras_constructor_forces_quality_and_sell_in():
    item = Item("Sulfuras, Hand of Ragnaros", sell_in=999, quality=10)
    assert item.sell_in == 0
    assert item.quality == 80


def test_sulfuras_multiple_days_unchanged():
    item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
    for _ in range(10):
        item.daily_step()
    assert item.sell_in == 0
    assert item.quality == 80


def test_sulfuras_mapping_via_item_new():
    # Ensure mapping creates Sulfuras instance
    item = Item("Sulfuras, Hand of Ragnaros", sell_in=1, quality=80)
    assert isinstance(item, Sulfuras)


def test_sulfuras_ignores_input_quality_not_80():
    # Even if we try to pass a non-80 quality, constructor forces 80
    item = Item("Sulfuras, Hand of Ragnaros", sell_in=1, quality=0)
    assert item.quality == 80

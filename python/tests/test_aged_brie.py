import pytest

from gilded_rose import Item, AgedBrie


def test_aged_brie_increases_quality_by_1_before_sell_date():
    # constructing via Item maps to AgedBrie automatically
    item = Item("Aged Brie", sell_in=10, quality=10)
    item.daily_step()
    assert isinstance(item, AgedBrie)
    assert item.sell_in == 9
    assert item.quality == 11


def test_aged_brie_increases_quality_by_2_after_sell_date():
    item = Item("Aged Brie", sell_in=0, quality=10)
    item.daily_step()
    assert item.sell_in == -1
    assert item.quality == 12


def test_aged_brie_quality_capped_at_50():
    item = Item("Aged Brie", sell_in=5, quality=49)
    item.daily_step()
    assert item.quality == 50
    item.daily_step()
    assert item.quality == 50


def test_aged_brie_stays_at_50_if_already_max():
    item = Item("Aged Brie", sell_in=5, quality=50)
    item.daily_step()
    assert item.quality == 50


def test_aged_brie_init_over_50_raises():
    with pytest.raises(AssertionError):
        # AgedBrie inherits Item constraints: must be <= 50
        Item("Aged Brie", sell_in=5, quality=51)

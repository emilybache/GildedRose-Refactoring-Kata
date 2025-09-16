import pytest

from gilded_rose import Item


def test_item_decreases_quality_and_sell_in():
    item = Item("foo", sell_in=10, quality=20)
    item.daily_step()
    assert item.sell_in == 9
    assert item.quality == 19


def test_item_quality_never_negative():
    item = Item("bar", sell_in=5, quality=0)
    item.daily_step()
    assert item.quality == 0


def test_item_degrades_twice_as_fast_after_sell_date():
    item = Item("baz", sell_in=0, quality=10)
    # After sell-in passes, degradation doubles
    item.daily_step()
    assert item.sell_in == -1
    assert item.quality == 8


def test_item_quality_floor_boundary():
    item = Item("qux", sell_in=1, quality=1)
    item.daily_step()
    assert item.quality == 0
    item.daily_step()  # now past sell date, but cannot drop below 0
    assert item.quality == 0


def test_item_init_with_negative_quality_raises():
    with pytest.raises(AssertionError):
        Item("oops", sell_in=5, quality=-1)

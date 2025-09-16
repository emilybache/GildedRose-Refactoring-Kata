import pytest

from gilded_rose import Item, BackstagePasses


def test_backstage_passes_increase_by_1_when_more_than_10_days():
    item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=10)
    item.daily_step()
    assert isinstance(item, BackstagePasses)
    assert item.sell_in == 14
    assert item.quality == 11


def test_backstage_passes_increase_by_2_between_10_and_6_days():
    item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=10)
    item.daily_step()
    assert item.sell_in == 9
    assert item.quality == 12


def test_backstage_passes_increase_by_3_between_5_and_1_days():
    item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=10)
    item.daily_step()
    assert item.sell_in == 4
    assert item.quality == 13


def test_backstage_passes_drop_to_zero_after_concert():
    item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=40)
    item.daily_step()
    assert item.sell_in == -1
    assert item.quality == 0


def test_backstage_passes_quality_capped_at_50():
    # At 5 days, would add +3 but must cap at 50
    item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)
    item.daily_step()
    assert item.quality == 50

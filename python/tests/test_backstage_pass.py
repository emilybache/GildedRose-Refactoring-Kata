from gilded_rose import Item, GildedRose

BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"

# Backstage passes, increases in Quality as its SellIn value approaches
def test_backstage_passes_increase_quality():
    items = [Item(BACKSTAGE_PASSES, 11, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 1

# Quality increases by 2 when there are 10 days or less 
def test_backstage_passes_increase_quality_by_2():
    items = [Item(BACKSTAGE_PASSES, 10, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 2

# Quality increases by 3 when there are 5 days or less
def test_backstage_passes_increase_quality_by_3():
    items = [Item(BACKSTAGE_PASSES, 5, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 3

# Quality drops to 0 after the concert
def test_backstage_passes_quality_is_0_sell_in_is_0():
    items = [Item(BACKSTAGE_PASSES, 0, 10)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 0
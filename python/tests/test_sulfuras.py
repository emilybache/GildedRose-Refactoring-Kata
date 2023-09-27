from gilded_rose import Item, GildedRose

SULFURAS = "Sulfuras, Hand of Ragnaros"

# "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
def test_sulfuras_sell_in_does_not_decrease():
    items = [Item(SULFURAS, 0, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].sell_in == 0

def test_sulfuras_does_not_decrease_in_quality():
    items = [Item(SULFURAS, 0, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 0
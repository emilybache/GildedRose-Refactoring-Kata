from gilded_rose import Item, GildedRose
CONJURED = "Conjured Mana Cake"

# "Conjured" items degrade in Quality twice as fast as normal items
def test_conjured_item_degrades_twice():
    items = [Item(CONJURED, 1, 12)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 10

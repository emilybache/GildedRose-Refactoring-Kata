from gilded_rose import Item, GildedRose

AGED_BRIE = "Aged Brie"
# "Aged Brie" actually increases in Quality the older it get
def test_aged_brie_increases_quality_older():
    items = [Item(AGED_BRIE, 0, 0)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()
    assert items[0].quality == 2
from gilded_rose import Item, GildedRose, AgedBrie, BackstagePasses, Sulfuras, Conjured


def test_gilded_rose_updates_normal_item():
    items = [Item("Normal", sell_in=2, quality=5)]
    gr = GildedRose(items)
    gr.update_quality()
    assert items[0].sell_in == 1
    assert items[0].quality == 4


def test_gilded_rose_updates_aged_brie():
    items = [Item("Aged Brie", sell_in=2, quality=0)]
    gr = GildedRose(items)
    gr.update_quality()
    assert isinstance(items[0], AgedBrie)
    assert items[0].sell_in == 1
    assert items[0].quality == 1


def test_gilded_rose_updates_backstage_passes_near_concert():
    items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=10)]
    gr = GildedRose(items)
    gr.update_quality()
    assert isinstance(items[0], BackstagePasses)
    assert items[0].sell_in == 4
    assert items[0].quality == 13


def test_gilded_rose_keeps_sulfuras_constant():
    items = [Item("Sulfuras, Hand of Ragnaros", sell_in=10, quality=80)]
    gr = GildedRose(items)
    gr.update_quality()
    assert isinstance(items[0], Sulfuras)
    assert items[0].sell_in == 0
    assert items[0].quality == 80


def test_gilded_rose_updates_mixed_items():
    items = [
        Item("Normal", sell_in=1, quality=2),
        Item("Aged Brie", sell_in=1, quality=49),
        Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=48),
        Item("Conjured Mana Cake", sell_in=3, quality=6),
        Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80),
    ]
    gr = GildedRose(items)
    gr.update_quality()

    # Normal
    assert items[0].sell_in == 0 and items[0].quality == 1

    # Aged Brie capped at 50
    assert items[1].sell_in == 0 and items[1].quality == 50

    # Backstage +2 at 10 days, capped at 50
    assert items[2].sell_in == 9 and items[2].quality == 50

    # Conjured -2
    assert isinstance(items[3], Conjured)
    assert items[3].sell_in == 2 and items[3].quality == 4

    # Sulfuras constant
    assert items[4].sell_in == 0 and items[4].quality == 80

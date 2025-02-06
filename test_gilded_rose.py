from gilded_rose import Item, GildedRose
from approvaltests.combination_approvals import verify_all_combinations


def test_update_quality():
    input_names = [
        "Normal",
        "Conjured",
        "Aged Brie",
        "Backstage passes to a TAFKAL80ETC concert",
        "Sulfuras, Hand of Ragnaros",
    ]
    input_qualities = [-1, 0, 1, 49, 50, 51]
    input_sell_ins = [-1, 0, 1, 10, 11, 12]

    verify_all_combinations(
        do_stuff,
        [
            input_names,
            input_sell_ins,
            input_qualities,
        ],
    )


def do_stuff(name, sell_in, quality):
    item = Item(name, sell_in, quality)
    GildedRose().update_quality([item])
    return str(item)

import unittest

from gilded_rose import Item, GildedRose
from approvaltests.combination_approvals import verify_all_combinations


class GildedRoseTest(unittest.TestCase):
    def do_stuff(self, name, sell_in=0, quality=0):
        item = Item(name, sell_in, quality)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        return str(item)

    def test_update_quality(self):
        input_names = [
            "foo",
            "Aged Brie",
            "Backstage passes to a TAFKAL80ETC concert",
            "Sulfuras, Hand of Ragnaros",
        ]
        input_qualities = [-1, 0, 1, 49, 50, 51]
        input_sell_ins = [-1, 0, 1, 10, 11, 12]

        verify_all_combinations(
            self.do_stuff,
            [
                input_names,
                input_sell_ins,
                input_qualities,
            ],
        )

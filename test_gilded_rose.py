import unittest

from gilded_rose import Item, GildedRose
from approvaltests.combination_approvals import verify_all_combinations


class GildedRoseTest(unittest.TestCase):
    def do_stuff(self, name, quality=0, sell_in=0):
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

        verify_all_combinations(self.do_stuff, [input_names, input_qualities, [0]])


if __name__ == "__main__":
    unittest.main()

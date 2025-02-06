import unittest

from gilded_rose import Item, GildedRose
from approvaltests.combination_approvals import verify_all_combinations


class GildedRoseTest(unittest.TestCase):
    def do_stuff(self, name, quality=0):
        item = Item(name, 0, quality)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        return str(item)

    def test_foo(self):
        input_names = [
            "foo",
            "Aged Brie",
            "Backstage passes to a TAFKAL80ETC concert",
            "Sulfuras, Hand of Ragnaros",
        ]

        verify_all_combinations(self.do_stuff, [input_names, [-1, 0, 1, 49, 50, 51]])


if __name__ == "__main__":
    unittest.main()

import unittest

from gilded_rose import Item, GildedRose
from approvaltests.approvals import verify
from approvaltests.combination_approvals import verify_all_combinations


class GildedRoseTest(unittest.TestCase):
    def do_stuff(self, name):
        item = Item(name, 0, 0)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        return str(item)

    def test_foo(self):
        input_vals = [
            "foo",
            "Aged Brie",
            "Backstage passes to a TAFKAL80ETC concert",
        ]

        verify_all_combinations(self.do_stuff, [input_vals])

        # to_approve = []
        # for item in input_vals:
        #     to_approve.append(self.do_stuff(item))
        #
        # verify("\n".join(to_approve))


if __name__ == "__main__":
    unittest.main()

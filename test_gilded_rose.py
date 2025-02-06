from copy import deepcopy
import unittest

from gilded_rose import Item, GildedRose
from approvaltests.approvals import verify


class GildedRoseTest(unittest.TestCase):
    def do_stuff(self, item):
        thing = Item(item[0], item[1], item[2])
        gilded_rose = GildedRose([thing])
        gilded_rose.update_quality()
        result = str(thing)
        return result

    def test_foo(self):
        input_Vals = [
            ("foo", 0, 0),
            ("Aged Brie", 0, 0),
            ("Backstage passes to a TAFKAL80ETC concert", 0, 0),
        ]

        to_approve = []
        for item in input_Vals:
            to_approve.append(self.do_stuff(item))

        verify("\n".join(to_approve))


if __name__ == "__main__":
    unittest.main()

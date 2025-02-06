from copy import deepcopy
import unittest

from gilded_rose import Item, GildedRose
from approvaltests.approvals import verify


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        inputs = [
            Item("foo", 0, 0),
            Item("Aged Brie", 0, 0),
            Item("Backstage passes to a TAFKAL80ETC concert", 0, 0),
        ]
        output_items = deepcopy(inputs)
        gilded_rose = GildedRose(output_items)
        gilded_rose.update_quality()

        to_approve = []
        for item in output_items:
            to_approve.append(str(item))

        verify("\n".join(to_approve))


if __name__ == "__main__":
    unittest.main()

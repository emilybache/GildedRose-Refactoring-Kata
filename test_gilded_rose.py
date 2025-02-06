import unittest

from gilded_rose import Item, GildedRose
from approvaltests.approvals import verify


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        to_approve = []
        for i in items:
            item = str(items[0])
            to_approve.append(item)
        verify("\n".join(to_approve))


if __name__ == "__main__":
    unittest.main()

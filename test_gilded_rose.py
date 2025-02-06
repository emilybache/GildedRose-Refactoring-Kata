import unittest

from gilded_rose import Item, GildedRose
from approvaltests.approvals import verify


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        verify(items[0].name)


if __name__ == "__main__":
    unittest.main()

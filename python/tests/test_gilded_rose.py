# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        '''
        
        '''
        # Error showed 'fixme' != 'foo' - item names don't change
        self.assertEqual("foo", items[0].name)
        # decreases by 1 each day (0 -> -1)
        self.assertEqual(-1, items[0].sell_in)
        # (cannot go below 0)
        self.assertEqual(0, items[0].quality)

if __name__ == '__main__':
    unittest.main()

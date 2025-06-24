# -*- coding: utf-8 -*-
import unittest
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__) + '/../'))

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        # The name should remain unchanged, but quality and sell_in should decrease
        self.assertEqual("foo", items[0].name)
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)

        
if __name__ == '__main__':
    unittest.main()

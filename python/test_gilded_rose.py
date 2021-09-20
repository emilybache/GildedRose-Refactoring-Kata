# -*- coding: utf-8 -*-
import unittest

from gilded_rose import *
from item import *


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [General("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("foo", items[0].name)

        
if __name__ == '__main__':
    unittest.main()

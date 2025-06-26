# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        #self.assertEqual("fixme", items[0].name) #this line was causing an error so commented this
        self.assertEqual("foo", items[0].name) #instead added this line
    
    def test_conjured_before_sell_date(): #added this function
        item = Item("Raj", 5, 10)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.quality, 8)

    def test_conjured_after_sell_date():  #added this function
        item = Item("Raj", 0, 10)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.quality, 6)

        
if __name__ == '__main__':
    unittest.main()

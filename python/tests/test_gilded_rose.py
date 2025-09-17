# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        # Error showed 'fixme' != 'foo' - item names don't change
        self.assertEqual("foo", items[0].name)
        # decreases by 1 each day (0 -> -1)
        self.assertEqual(-1, items[0].sell_in)
        # (cannot go below 0)
        self.assertEqual(0, items[0].quality)

    def test_aged_brie_increases_quality_before_sell_date(self):
        items = [Item("Aged Brie", 2, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(1, items[0].quality)  # +1
    
    def test_aged_brie_increases_quality_on_sell_date(self):
        items = [Item("Aged Brie", 1, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(11, items[0].quality)  # +1
    def test_aged_brie_increases_quality_twice_after_sell_date(self):
        items = [Item("Aged Brie", 0, 10)]  # Will become sell_in = -1
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(12, items[0].quality)
    def test_aged_brie_quality_cannot_exceed_50(self):
        items = [Item("Aged Brie", 2, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(50, items[0].quality)  # Stays at 50
    def test_aged_brie_near_max_quality_after_sell_date(self):
        items = [Item("Aged Brie", 0, 49)]  #treis to add 2, but caps at 50
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(50, items[0].quality)  # 49 + 1 = 50 (second +1 blocked by cap)
    def test_sulfuras_quality_never_decreases(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(0, items[0].sell_in)   
        self.assertEqual(80, items[0].quality)
    def test_sulfuras_works_when_expired(self):
        items = [Item("Sulfuras, Hand of Ragnaros", -1, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)  
        self.assertEqual(80, items[0].quality)
    def test_sulfuras_ignores_quality_limits(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 5, 100)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(5, items[0].sell_in)   # No change 
        self.assertEqual(100, items[0].quality)  
    
    
    
    
    


if __name__ == '__main__':
    unittest.main()

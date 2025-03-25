# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
                if item.quality > 0:
                    if item.name != "Sulfuras, Hand of Ragnaros":
                        item.quality = item.quality - 1
            else:
                if item.quality < 50:
                    item.quality = item.quality + 1
                    if item.name == "Backstage passes to a TAFKAL80ETC concert":
                        if item.sell_in < 11:
                            if item.quality < 50:
                                item.quality = item.quality + 1
                        if item.sell_in < 6:
                            if item.quality < 50:
                                item.quality = item.quality + 1
            if item.name != "Sulfuras, Hand of Ragnaros":
                item.sell_in = item.sell_in - 1
            if item.sell_in < 0:
                if item.name != "Aged Brie":
                    if item.name != "Backstage passes to a TAFKAL80ETC concert":
                        if item.quality > 0:
                            if item.name != "Sulfuras, Hand of Ragnaros":
                                item.quality = item.quality - 1
                    else:
                        item.quality = item.quality - item.quality
                else:
                    if item.quality < 50:
                        item.quality = item.quality + 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# Testing code for Conjured items # SATTI YESUREDDY
import unittest

class TestGildedRose(unittest.TestCase):
    
    def test_conjured_item_quality_degrades_twice_as_fast(self):
        conjured_item = Item("Conjured Mana Cake", 10, 20)
        gilded_rose = GildedRose([conjured_item])
        
        gilded_rose.update_quality()
        self.assertEqual(conjured_item.quality, 18)
        
        gilded_rose.update_quality()
        self.assertEqual(conjured_item.quality, 16)
    
    def test_conjured_item_quality_does_not_go_below_zero(self):
        conjured_item = Item("Conjured Mana Cake", 10, 1)
        gilded_rose = GildedRose([conjured_item])
        
        gilded_rose.update_quality()
        self.assertEqual(conjured_item.quality, 0) 

if __name__ == "__main__":
    unittest.main()

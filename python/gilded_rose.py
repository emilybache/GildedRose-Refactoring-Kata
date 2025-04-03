# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self.update_sell_in(item)
            self.update_item_quality(item)

    def update_sell_in(self, item):
        if item.name != "Sulfuras, Hand of Ragnaros": 
            item.days_left -= 1


    def update_item_quality(self,item):
        if item.name == "Aged Brie":
            self.update_aged_brie_quality(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            self.update_backstage_passes_quality(item)
        elif item.name == "Sulfuras, Hand of Ragnaros":
            pass
        else:
            self.update_normal_item_quality(item)

    def update_normal_item_quality(self,item):
        if item.quality >0:
            item.quality -=1

        if item.days_left < 0 and item.quality > 0:
            item.quality -= 1

    def update_aged_brie_quality(self, item):
        if item.quality < 50:
            item.quality += 1
        if item.days_left < 0 and item.quality < 50:
            item.quality += 1


    def update_backstage_passes_quality(self, item):
        if item.days_left < 0:
            item.quality = 0
        elif item.days_left < 5:
            item.quality += 3
        elif item.days_left < 10:
            item.quality += 2
        else:
            item.quality += 1
        if item.quality > 50:
            item.quality = 50


class Item:
    def __init__(self, name, days_left, quality):
        self.name = name
        self.days_left = days_left
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.days_left, self.quality)




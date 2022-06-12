# -*- coding: utf-8 -*-

class GildedRose(object):

    AGED_BRIE = "Aged Brie"
    BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
    SULFURAS = "Sulfuras, Hand of Ragnaros"
    qualityIncrease = 1

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self.update_item_quality(item)

    def update_item_quality(self, item):
        
        if item.name != self.AGED_BRIE and item.name != self.BACKSTAGE_PASSES:
            if item.quality > 0:
                
                if item.name != self.SULFURAS:
                    self.adjust_quality(item, - self.qualityIncrease)
        else:
            if item.quality < 50:
                self.adjust_quality(item, self.qualityIncrease)
                if item.name == self.BACKSTAGE_PASSES:
                    if item.sell_in < 11:
                        self.adjust_quality(item, self.qualityIncrease)
                    if item.sell_in < 6:
                        self.adjust_quality(item, self.qualityIncrease)
        if item.name != self.SULFURAS:
            item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            if item.name != self.AGED_BRIE:
                if item.name != self.BACKSTAGE_PASSES:
                    if item.quality > 0:
                        if item.name != self.SULFURAS:
                            self.adjust_quality(item, - self.qualityIncrease)
                else:
                    item.quality = item.quality - item.quality
            else:
                if item.quality < 50:
                    self.adjust_quality(item, self.qualityIncrease)

    # Update the quality item value if the quality value is within the range 0 to 50
    def adjust_quality(self, item, quality_increase_decrease):
        new_quality = item.quality + quality_increase_decrease
        if new_quality >= 0 and new_quality <= 50:
            item.quality = new_quality



class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

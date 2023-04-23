# -*- coding: utf-8 -*-
SULFURAS = "Sulfuras, Hand of Ragnaros"
BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
AGED_BRIE = "Aged Brie"


class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            increment = 1
            if item.name != AGED_BRIE and item.name != BACKSTAGE_PASSES and item.name != SULFURAS:
                self.regulateQuality(-increment, item)
            else:
                self.regulateQuality(increment, item)
                if item.name == BACKSTAGE_PASSES:
                    if item.sell_in < 11 or item.sell_in < 6:
                        self.regulateQuality(increment, item)

            if item.name != SULFURAS:
                item.sell_in = item.sell_in - increment
            if item.sell_in < 0:
                if item.name != AGED_BRIE and item.name != BACKSTAGE_PASSES and item.name != SULFURAS:
                    self.regulateQuality(-increment, item)
                else:
                    if item.name == BACKSTAGE_PASSES:
                        item.quality = item.quality - item.quality
                    elif item.name == AGED_BRIE:
                        self.regulateQuality(increment, item)

    def regulateQuality(self, increment, item):
        newQuality = item.quality + increment
        if 0 <= newQuality <= 50:
            item.quality = newQuality


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

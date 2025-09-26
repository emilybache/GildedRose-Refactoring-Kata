# -*- coding: utf-8 -*-
MAX_QUALITY = 50
MIN_QUALITY = 0

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            update_function = STRATEGIES.get(item.name, STRATEGIES["Normal Item"])
            update_function(item)


def update_quality_sulfuras(item):
    pass    

def update_quality_aged_brie(item):
    increase_quality(item)
    item.sell_in -= 1
    if item.sell_in < 0:
        increase_quality(item)

def update_quality_backstage_pass(item):
    increase_quality(item)
    if item.sell_in < 11:
        increase_quality(item)
    if item.sell_in < 6:
        increase_quality(item)
    item.sell_in -= 1
    if item.sell_in < 0:
        item.quality = 0

def update_quality_conjured(item):
    decrease_quality(item)
    decrease_quality(item)
    item.sell_in -= 1
    if item.sell_in < 0:
        decrease_quality(item)
        decrease_quality(item)

def update_quality_normal(item):
    decrease_quality(item)
    item.sell_in -= 1
    if item.sell_in < 0:
        decrease_quality(item)


STRATEGIES = {
"Sulfuras, Hand of Ragnaros": update_quality_sulfuras,
"Aged Brie": update_quality_aged_brie,
"Backstage passes to a TAFKAL80ETC concert": update_quality_backstage_pass,
"Conjured Mana Cake": update_quality_conjured,
"Normal Item": update_quality_normal
}



def increase_quality(item):
    item.quality = min(MAX_QUALITY, item.quality + 1)
def decrease_quality(item):
    item.quality = max(MIN_QUALITY, item.quality - 1)

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

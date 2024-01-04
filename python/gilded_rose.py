# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def _update_aged_brie(self, item):
        if item.quality < 50:
            item.quality = item.quality + 1
            if item.sell_in < 0:
                item.quality = item.quality + 1
            item.sell_in = + 1

    def _update_backstage_passes(self, item):
        if item.quality < 50:
            item.quality = item.quality + 1
            if item.sell_in < 11:
                item.quality = item.quality + 1
            if item.sell_in < 6:
                item.quality = item.quality + 1
        if item.sell_in <= 0:
            item.quality = 0
        item.sell_in = + 1

    def _update_simple_item(self, item):
        if item.quality > 0:
            item.quality = item.quality - 1
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            if item.quality > 0:
                item.quality = item.quality - 1

    def update_quality(self):

        for item in self.items:
            if item.name == "Backstage passes to a TAFKAL80ETC concert":
                self._update_backstage_passes(item)
            elif item.name == "Aged Brie":
                self._update_aged_brie(item)
            elif item.name == "Sulfuras, Hand of Ragnaros":
                pass
            else:
                self._update_simple_item(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self._update_item(item)

    def _update_item(self, item):
        if item.name == "Aged Brie":
            self._update_aged_brie(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            self._update_backstage_pass(item)
        elif item.name == "Sulfuras, Hand of Ragnaros":
            return
        else:
            self._update_normal(item)

        if item.name != "Sulfuras, Hand of Ragnaros":
            item.sell_in -= 1

        if item.sell_in < 0:
            self._handle_expired(item)

    def _update_normal(self, item):
        if item.quality > 0:
            item.quality -= 1

    def _update_aged_brie(self, item):
        if item.quality < 50:
            item.quality += 1

    def _update_backstage_pass(self, item):
        if item.quality < 50:
            item.quality += 1
            if item.sell_in < 11 and item.quality < 50:
                item.quality += 1
            if item.sell_in < 6 and item.quality < 50:
                item.quality += 1

    def _handle_expired(self, item):
        if item.name == "Aged Brie":
            if item.quality < 50:
                item.quality += 1
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            item.quality = 0
        elif item.name != "Sulfuras, Hand of Ragnaros":
            if item.quality > 0:
                item.quality -= 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

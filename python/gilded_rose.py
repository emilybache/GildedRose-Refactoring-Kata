# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def adjust_quality(self, item, rate=-1):
        """
        Adjust the quality of an item, defaults to -1
        Quality cannot be less than 0 or bumped beyond 50
        """
        if item.quality <= 0 or item.quality >= 50:
            return
        item.quality += rate

    def update_quality(self):
        for item in self.items:
            if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
                if item.name != "Sulfuras, Hand of Ragnaros":
                    self.adjust_quality(item)

            else:
                if item.quality < 50:
                    self.adjust_quality(item, 1)
                    if item.name == "Backstage passes to a TAFKAL80ETC concert":
                        if item.sell_in < 11:
                            if item.quality < 50:
                                self.adjust_quality(item, 1)
                        if item.sell_in < 6:
                            if item.quality < 50:
                                self.adjust_quality(item, 1)
            if item.name != "Sulfuras, Hand of Ragnaros":
                item.sell_in = item.sell_in - 1
            if item.sell_in < 0:
                if item.name != "Aged Brie":
                    if item.name != "Backstage passes to a TAFKAL80ETC concert":
                        if item.name != "Sulfuras, Hand of Ragnaros":
                            self.adjust_quality(item)
                    else:
                        item.quality = 0
                else:
                    self.adjust_quality(item, 1)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

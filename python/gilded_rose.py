# -*- coding: utf-8 -*-

def decrease_quality(item, decrease_quality_by=None):
    if not decrease_quality_by:
        if item.sell_in <= 0:
            decrease_quality_by = 2
        else:
            decrease_quality_by = 1

    item.quality = max(item.quality - decrease_quality_by, 0)


def increase_quality(item, increase_quality_by=1):
    item.quality = min(item.quality + increase_quality_by, 50)


class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            if item.name == "Sulfuras, Hand of Ragnaros":
                continue
            elif item.name == "Aged Brie":
                increase_quality(item)
            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                if item.sell_in <= 0:
                    item.quality = 0
                elif item.sell_in <= 5:
                    increase_quality(item, 3)
                elif item.sell_in <= 10:
                    increase_quality(item, 2)
                else:
                    increase_quality(item)
            else:
                decrease_quality(item)

            item.sell_in = item.sell_in - 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

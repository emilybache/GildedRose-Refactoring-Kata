# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items
        self.AGED_BRIE = "Aged Brie"
        self.SULFURAS = "Sulfuras, Hand of Ragnaros"
        self.BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"

    def update_quality(self):
        for item in self.items:
            self.__update_each_item_quality(item)
            # print(f'Log: {item}\n')

    def __update_each_item_quality(self, item):
        # Update item.sell_in
        if item.name != self.SULFURAS:
            item.sell_in = item.sell_in - 1
        # Update item.quality
        if item.name != self.AGED_BRIE and item.name != self.BACKSTAGE_PASSES:
            if item.quality > 0:
                if item.name != self.SULFURAS:
                    item.quality = item.quality - 1
        else:
            if item.quality < 50:
                item.quality = item.quality + 1
                if item.name == self.BACKSTAGE_PASSES:
                    if item.sell_in < 10:
                        if item.quality < 50:
                            item.quality = item.quality + 1
                    if item.sell_in < 5:
                        if item.quality < 50:
                            item.quality = item.quality + 1
        if item.sell_in < 0:
            if item.name != self.AGED_BRIE:
                if item.name != self.BACKSTAGE_PASSES:
                    if item.quality > 0:
                        if item.name != self.SULFURAS:
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
        return f"{self.name}, {self.sell_in}, {self.quality}"

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

    def __decrease_item_quality(self, item, amount: int = 1):
        item.quality = max(0, item.quality - amount)

    def __increase_item_quality(self, item, amount: int = 1):
        item.quality = min(50, item.quality + amount)

    def __update_each_item_quality(self, item):
        # Update item.sell_in
        if item.name == self.SULFURAS:
            pass
        else:
            item.sell_in = item.sell_in - 1
        # Update item.quality
        if item.name == self.AGED_BRIE:
            self.__increase_item_quality(item)
            if item.sell_in < 0:
                self.__increase_item_quality(item)
        elif item.name == self.BACKSTAGE_PASSES:
            self.__increase_item_quality(item)
            if item.sell_in < 10:
                self.__increase_item_quality(item)
            if item.sell_in < 5:
                self.__increase_item_quality(item)
            if item.sell_in < 0:
                item.quality = 0
        elif item.name == self.SULFURAS:
            pass
        else:
            self.__decrease_item_quality(item)
            if item.sell_in < 0:
                self.__decrease_item_quality(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return f"{self.name}, {self.sell_in}, {self.quality}"

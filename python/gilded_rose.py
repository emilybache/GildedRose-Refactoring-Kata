# -*- coding: utf-8 -*-
from item_handler import (
    AgedBrieHandler,
    BackstagePassesHandler,
    DefaultItemHandler,
    SulfurasHandler,
    ConjuredItemHandler)


class GildedRose(object):

    def __init__(self, items):
        self.items = items
        self.AGED_BRIE = "Aged Brie"
        self.SULFURAS = "Sulfuras, Hand of Ragnaros"
        self.BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
        self.CONJURED = "Conjured Mana Cake"

    def update_quality(self):
        for item in self.items:
            self.__update_each_item_quality(item)
            # print(f'Log: {item}\n')

    def __update_each_item_quality(self, item):
        ITEM_SWITCH = {
            self.AGED_BRIE: AgedBrieHandler(),
            self.SULFURAS: SulfurasHandler(),
            self.BACKSTAGE_PASSES: BackstagePassesHandler(),
            self.CONJURED: ConjuredItemHandler()
        }
        item.handler = ITEM_SWITCH.get(item.name, DefaultItemHandler())

        item.handler.update_sell_in(item)
        item.handler.update_quality(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return f"{self.name}, {self.sell_in}, {self.quality}"

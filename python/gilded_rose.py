# -*- coding: utf-8 -*-

from dataclasses import dataclass
from typing import ClassVar, Dict, Type

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            updater = ItemUpdater.update_for(item)
            updater.update()

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

@dataclass()
class ItemUpdater:

    item: Item

    _registry: ClassVar[Dict[str, Type["ItemUpdater"]]] = {}

    @classmethod
    def register(cls, name: str):
        def deco(subcls: Type["ItemUpdater"]) -> Type["ItemUpdater"]:
            cls._registry[name] = subcls
            return subcls
        return deco

    @classmethod
    def update_for(cls, item: Item) -> "ItemUpdater":
        updater_cls = cls._registry.get(item.name, cls)
        return updater_cls(item)


    def update(self):
        self.item.sell_in = self.item.sell_in - 1

        if self.item.quality > 0:
            self.item.quality = self.item.quality - 1

        if self.item.sell_in < 0 and self.item.quality > 0:
            self.item.quality = self.item.quality - 1


@ItemUpdater.register("Aged Brie")
class AgedBrieUpdater(ItemUpdater):

    def update(self):
        if self.item.quality < 50:
            self.item.quality = self.item.quality + 1

        self.item.sell_in = self.item.sell_in - 1

        if self.item.sell_in < 0:
            if self.item.quality < 50:
                self.item.quality = self.item.quality + 1

@ItemUpdater.register("Sulfuras, Hand of Ragnaros")
class SulfurasUpdater(ItemUpdater):

    def update(self):
        pass # Legendary item, never changes

@ItemUpdater.register("Backstage passes to a TAFKAL80ETC concert")
class BackstagePassUpdater(ItemUpdater):

    def update(self):
        if self.item.sell_in < 1:
            self.item.quality = 0  # Concert passed, quality drops to 0
        elif self.item.sell_in < 6:
            self.item.quality = min(50, self.item.quality + 3)  # 5 days or less: +3
        elif self.item.sell_in < 11:
            self.item.quality = min(50, self.item.quality + 2)  # 10 days or less: +2
        else:
            self.item.quality = min(50, self.item.quality + 1)  # More than 10 days: +1

        self.item.sell_in = self.item.sell_in - 1

# -*- coding: utf-8 -*-
from items import Item

class BaseItemUpdater:
    def __init__(self, item):
        self.item = item

    def update(self):
        self.update_sell_in()
        self.update_quality()

    def update_sell_in(self):
        self.item.sell_in -= 1

    def update_quality(self):
        degrade = 1
        if self.item.sell_in < 0:
            degrade = 2
        self.item.quality = max(self.item.quality - degrade, 0)



class AgedBrieUpdater(BaseItemUpdater):
    def update_quality(self):
        if self.item.quality < 50:
            self.item.quality += 1
        if self.item.sell_in < 0 and self.item.quality < 50:
            self.item.quality += 1



class BackstagePassUpdater(BaseItemUpdater):
    def update_quality(self):
        if self.item.sell_in < 0:
            self.item.quality = 0
        elif self.item.sell_in < 5:
            self.item.quality = min(self.item.quality + 3, 50)
        elif self.item.sell_in < 10:
            self.item.quality = min(self.item.quality + 2, 50)
        else:
            if self.item.quality < 50:
                self.item.quality += 1


class SulfurasUpdater(BaseItemUpdater):
    def update(self):
        print("Sulfuras has no change")
    def update_quality(self):
        print("Sulfuras has no change")
        # pass  
    def update_sell_in(self):
        print("Sulfuras has no change")

class ConjuredItemUpdater(BaseItemUpdater):
    def update_quality(self):
        degrade = 2
        if self.item.sell_in < 0:
            degrade *= 2
        self.item.quality = max(self.item.quality - degrade, 0)


class ItemUpdaterFactory:
    @staticmethod
    def get_updater(item):
        if item.name == "Aged Brie":
            return AgedBrieUpdater(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            return BackstagePassUpdater(item)
        elif item.name == "Sulfuras, Hand of Ragnaros":
            return SulfurasUpdater(item)
        elif item.name == "Conjured Mana Cake": 
            return ConjuredItemUpdater(item)
        else:
            return BaseItemUpdater(item)


class GildedRose:
    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            updater = ItemUpdaterFactory.get_updater(item)
            updater.update()

# class GildedRose(object):

#     def __init__(self, items):
#         self.items = items

#     def update_quality(self):
#         for item in self.items:
#             if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
#                 if item.quality > 0:
#                     if item.name != "Sulfuras, Hand of Ragnaros":
#                         item.quality = item.quality - 1
#             else:
#                 if item.quality < 50:
#                     item.quality = item.quality + 1
#                     if item.name == "Backstage passes to a TAFKAL80ETC concert":
#                         if item.sell_in < 11:
#                             if item.quality < 50:
#                                 item.quality = item.quality + 1
#                         if item.sell_in < 6:
#                             if item.quality < 50:
#                                 item.quality = item.quality + 1
#             if item.name != "Sulfuras, Hand of Ragnaros":
#                 item.sell_in = item.sell_in - 1
#             if item.sell_in < 0:
#                 if item.name != "Aged Brie":
#                     if item.name != "Backstage passes to a TAFKAL80ETC concert":
#                         if item.quality > 0:
#                             if item.name != "Sulfuras, Hand of Ragnaros":
#                                 item.quality = item.quality - 1
#                     else:
#                         item.quality = item.quality - item.quality
#                 else:
#                     if item.quality < 50:
#                         item.quality = item.quality + 1


# class Item:
#     def __init__(self, name, sell_in, quality):
#         self.name = name
#         self.sell_in = sell_in
#         self.quality = quality

#     def __repr__(self):
#         return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# -*- coding: utf-8 -*-

class GildedRose(object):
    BACKSTAGE = 'Backstage passes to a TAFKAL80ETC concert'
    SULFURAS = 'Sulfuras, Hand of Ragnaros'
    AGED_BRIE = 'Aged Brie'
    MAX_QUAL = 50

    def __init__(self, items):
        self.items = items

    def __qual_inc(self, item, inc):
        item.quality = max(50, item.quality + inc)

    def __qual_dec(self, item, dec):
        item.quality = min(0, item.quality - dec)

    def update_quality(self):
        for item in self.items:
            match item.name:
                case self.AGED_BRIE:
                    pass
                case self.SULFURAS:
                    pass
                case self.BACKSTAGE:
                    pass
                case _:
                    self.__qual_dec(item, 1)
            if not item.name == self.SULFURAS:
                item.sell_in = item.sell_in - 1
            # if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
            #     if item.quality > 0:
            #         if item.name != "Sulfuras, Hand of Ragnaros":
            #             item.quality = item.quality - 1
            # else:
            #     if item.quality < 50:
            #         item.quality = item.quality + 1
            #         if item.name == "Backstage passes to a TAFKAL80ETC concert":
            #             if item.sell_in < 11:
            #                 if item.quality < 50:
            #                     item.quality = item.quality + 1
            #             if item.sell_in < 6:
            #                 if item.quality < 50:
            #                     item.quality = item.quality + 1
            # if item.name != "Sulfuras, Hand of Ragnaros":
            #     item.sell_in = item.sell_in - 1
            # if item.sell_in < 0:
            #     if item.name != "Aged Brie":
            #         if item.name != "Backstage passes to a TAFKAL80ETC concert":
            #             if item.quality > 0:
            #                 if item.name != "Sulfuras, Hand of Ragnaros":
            #                     item.quality = item.quality - 1
            #         else:
            #             item.quality = item.quality - item.quality
            #     else:
            #         if item.quality < 50:
            #             item.quality = item.quality + 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

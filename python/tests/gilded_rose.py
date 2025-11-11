# -*- coding: utf-8 -*-

class GildedRose(object):
    BACKSTAGE = 'Backstage passes to a TAFKAL80ETC concert'
    SULFURAS = 'Sulfuras, Hand of Ragnaros'
    AGED_BRIE = 'Aged Brie'
    MAX_QUAL = 50

    def __init__(self, items):
        self.items = items

    def __qual_inc(self, item, inc):
        item.quality = min(self.MAX_QUAL, item.quality + inc)

    def __qual_dec(self, item, dec):
        item.quality = max(0, item.quality - dec)

    def update_quality(self):
        for item in self.items:
            match item.name:
                case self.AGED_BRIE:
                    if item.sell_in > 0:
                        self.__qual_inc(item, 1)
                    else:
                        self.__qual_inc(item, 2)
                case self.SULFURAS:
                    pass
                case self.BACKSTAGE:
                    if item.sell_in > 10:
                        self.__qual_inc(item, 1)
                    elif item.sell_in > 5:
                        self.__qual_inc(item, 2)
                    elif item.sell_in > 0:
                        self.__qual_inc(item, 3)
                    else:
                        item.quality = 0
                case _:
                    multiplier = 1
                    if item.name.startswith('Conjured'):
                        multiplier = 2
                    if item.sell_in > 0:
                        self.__qual_dec(item, multiplier)
                    else:
                        self.__qual_dec(item, 2 * multiplier)
            if not item.name == self.SULFURAS:
                item.sell_in -= 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

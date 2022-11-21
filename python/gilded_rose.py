# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def adjust_quality(self, item, rate):
        """
        Adjust the quality of an item, defaults to -1
        Quality cannot be less than 0 or bumped beyond 50
        """
        if item.quality <= 0 or item.quality >= 50:
            return

        item.quality += rate

    def update_quality(self):
        for item in self.items:

            item.sell_in -= 1
            has_positive_sell_in = item.sell_in >= 0
            rate = -1

            if item.name == "Aged Brie":
                rate = 1 if has_positive_sell_in else 2
                return self.adjust_quality(item, rate)

            if item.name == "Backstage passes to a TAFKAL80ETC concert":
                if 5 <= item.sell_in <= 10:
                    rate = 2
                elif 0 <= item.sell_in <= 5:
                    rate = 3
                else:
                    rate = -item.quality
                return self.adjust_quality(item, rate)

            if "Conjured" in item.name:
                rate = -2 if has_positive_sell_in else -4
                return self.adjust_quality(item, rate)

            return self.adjust_quality(item, -1) if has_positive_sell_in else self.adjust_quality(item, -2)
            

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:

            # Set default parameters
            sell_in_rate = 1
            quality_rate = 1
            quality_sign = -1

            # Once the sell by date has passed, Quality degrades twice as fast
            if item.sell_in <= 0:
                quality_rate = 2

            # "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
            if item.name == "Sulfuras, Hand of Ragnaros":
                sell_in_rate = 0
                quality_rate = 0
                quality_sign = 0

            elif item.name == "Aged Brie":
                # "Aged Brie" actually increases in Quality the older it gets
                quality_sign = 1

            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                # Quality drops to 0 after the concert
                if item.sell_in <= 0:
                    item.quality -= item.quality
                    quality_sign = 0

                # Quality increases by 3 when there are 5 days or less
                elif item.sell_in <= 5:
                    quality_rate = 3
                    # "Backstage passes..." increases in quality
                    quality_sign = 1

                # Quality increases by 2 when there are 10 days or less
                elif item.sell_in <= 10:
                    quality_rate = 2
                    # "Backstage passes..." increases in quality
                    quality_sign = 1

                else:
                    quality_sign = 1

            item.sell_in -= sell_in_rate
            item.quality += (quality_sign*quality_rate)

            # CONDITIONS THAT ARE ALWAYS TRUE FOR EVERY ITEM
            # The Quality of an item is never negative
            if item.quality < 0:
                item.quality = 0

            # No items' quality can exceed 50, except 'Sulfuras, Hand of Ragnaros'
            if item.quality > 50 and item.name != "Sulfuras, Hand of Ragnaros":
                item.quality = 50


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:

            # Once the sell by date has passed, Quality degrades twice as fast
            if item.sell_in < 0:
                quality_rate = 2

            # The Quality of an item is never negative
            if item.quality < 0:
                item.quality = 0

            # "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
            if item.name == "Sulfuras, Hand of Ragnaros":
                sell_in_rate = 0
                quality_rate = 0
                quality_inc_dec = 0
            elif item.name == "Aged Brie":
                sell_in_rate = 1
                # "Aged Brie" actually increases in Quality the older it gets
                quality_inc_dec = 1
            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                sell_in_rate = 1
                # Quality drops to 0 after the concert
                if item.sell_in < 0:
                    item.quality -= item.quality
                # Quality increases by 3 when there are 5 days or less
                elif item.sell_in < 6:
                    quality_rate = 3
                    # "Backstage passes..." increases in quality
                    quality_inc_dec = 1
                # Quality increases by 2 when there are 10 days or less
                elif item.sell_in < 11:
                    quality_rate = 2
                    # "Backstage passes..." increases in quality
                    quality_inc_dec = 1
            elif item.name == "Conjured Mana Cake":
                sell_in_rate = 1
                quality_rate = 1
                quality_inc_dec = -1

                if item.sell_in < 0:
                    quality_rate = 2

            # Set default values for any unknown items
            else:
                sell_in_rate = 1
                quality_rate = 1
                quality_inc_dec = -1

            item.sell_in -= sell_in_rate
            item.quality += (quality_inc_dec*quality_rate)

            if item.quality > 50 and item.name != "Sulfuras, Hand of Ragnaros":
                item.quality = 50






            # # item.name is something other than "Aged Brie" or "Backstage passes to a TAFKAL80ETC concert"
            # if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
            #     if item.quality > 0:
            #
            #         # item.name is something other than "Sulfuras, Hand of Ragnaros"
            #         if item.name != "Sulfuras, Hand of Ragnaros":
            #             item.quality -= 1
            #
            # # item.name == "Aged Brie" OR "Backstage passes to a TAFKAL80ETC concert"
            # else:
            #     if item.quality < 50:
            #         item.quality += 1
            #         if item.name == "Backstage passes to a TAFKAL80ETC concert":
            #             if item.sell_in < 11:
            #                 if item.quality < 50:
            #                     item.quality += 1
            #             if item.sell_in < 6:
            #                 if item.quality < 50:
            #                     item.quality += 1
            #
            #
            # if item.name != "Sulfuras, Hand of Ragnaros":
            #     item.sell_in -= 1
            #
            #
            # if item.sell_in < 0:
            #     if item.name != "Aged Brie":
            #         if item.name != "Backstage passes to a TAFKAL80ETC concert":
            #             if item.quality > 0:
            #                 if item.name != "Sulfuras, Hand of Ragnaros":
            #                     item.quality -= 1
            #         else:
            #             item.quality -= item.quality
            #     else:
            #         if item.quality < 50:
            #             item.quality += 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

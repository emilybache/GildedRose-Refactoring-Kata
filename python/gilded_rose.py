# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            # Legendary item: no changes
            if item.name == "Sulfuras, Hand of Ragnaros":
                continue

            # Backstage passes have bespoke rules
            if item.name == "Backstage passes to a TAFKAL80ETC concert":
                if item.sell_in <= 0:
                    item.quality = 0
                else:
                    increment = 1
                    if item.sell_in <= 5:
                        increment = 3
                    elif item.sell_in <= 10:
                        increment = 2
                    item.quality = min(50, item.quality + increment)
                item.sell_in -= 1
                continue

            # Aged Brie increases in quality over time
            if item.name == "Aged Brie":
                increment = 1 if item.sell_in > 0 else 2
                item.quality = min(50, item.quality + increment)
                item.sell_in -= 1
                continue

            # Normal and Conjured items degrade
            is_conjured = item.name.lower().startswith("conjured")
            base_degradation = 2 if is_conjured else 1
            degradation = base_degradation
            if item.sell_in <= 0:
                degradation *= 2
            item.quality = max(0, item.quality - degradation)
            item.sell_in -= 1


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

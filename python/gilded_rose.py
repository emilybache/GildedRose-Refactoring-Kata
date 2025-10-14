# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def increase_quality(self, item, amount=1):
        """Increase item quality, capped at 50"""
        item.quality = min(50, item.quality + amount)

    def decrease_quality(self, item, amount=1):
        """Decrease item quality, never below 0"""
        item.quality = max(0, item.quality - amount)

    def update_normal(self, item):
        """Update quality for normal items"""
        self.decrease_quality(item)
        item.sell_in -= 1
        if item.sell_in < 0:
            self.decrease_quality(item)

    def update_aged_brie(self, item):
        """Update quality for Aged Brie"""
        self.increase_quality(item)
        item.sell_in -= 1
        if item.sell_in < 0:
            self.increase_quality(item)

    def update_backstage_pass(self, item):
        """Update quality for Backstage passes"""
        self.increase_quality(item)
        if item.sell_in <= 10:
            self.increase_quality(item)
        if item.sell_in <= 5:
            self.increase_quality(item)
        item.sell_in -= 1
        if item.sell_in < 0:
            item.quality = 0

    def update_conjured(self, item):
        """Update quality for Conjured items"""
        self.decrease_quality(item, 2)
        item.sell_in -= 1
        if item.sell_in < 0:
            self.decrease_quality(item, 2)

    def update_quality(self):
        for item in self.items:
            if item.name == "Sulfuras, Hand of Ragnaros":
                # Sulfuras never changes
                continue

            if item.name == "Aged Brie":
                self.update_aged_brie(item)
            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                self.update_backstage_pass(item)
            elif item.name.startswith("Conjured"):
                self.update_conjured(item)
            else:
                self.update_normal(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# -*- coding: utf-8 -*-

class GildedRose(object):
    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self.update_item_quality(item)
            self.update_sell_in(item)
            if item.sell_in < 0:
                self.handle_expired_item(item)

    def update_item_quality(self, item):
        if item.name == "Aged Brie":
            self.update_aged_brie(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            self.update_backstage_pass(item)
        elif "Conjured" in item.name:
            self.update_conjured_item(item)
        elif item.name != "Sulfuras, Hand of Ragnaros":
            self.update_normal_item(item)

    def update_conjured_item(self, item):
        self.decrease_quality(item)
        self.decrease_quality(item)

    def update_sell_in(self, item):
        if item.name != "Sulfuras, Hand of Ragnaros":
            item.sell_in -= 1

    def handle_expired_item(self, item):
        if item.name == "Aged Brie":
            self.increase_quality(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            item.quality = 0
        elif "Conjured" in item.name:
            self.decrease_quality(item)
            self.decrease_quality(item)
        elif item.name != "Sulfuras, Hand of Ragnaros":
            self.decrease_quality(item)

    def update_aged_brie(self, item):
        self.increase_quality(item)

    def update_backstage_pass(self, item):
        self.increase_quality(item)
        if item.sell_in < 11:
            self.increase_quality(item)
        if item.sell_in < 6:
            self.increase_quality(item)

    def update_normal_item(self, item):
        self.decrease_quality(item)

    def increase_quality(self, item):
        if item.quality < 50:
            item.quality += 1

    def decrease_quality(self, item):
        if item.quality > 0:
            item.quality -= 1

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

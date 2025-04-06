# -*- coding: utf-8 -*-

class ITEM_NAME:
    AGED_BRIE = "Aged Brie"
    BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
    SULFURAS = "Sulfuras, Hand of Ragnaros"
    CONJURED = "Conjured"


class GildedRose(object):
    MAX_QUALITY = 50
    LEGENDARY_QUALITY = 80
    LEGENDARY_ITEM = ITEM_NAME.SULFURAS
    BACKSTAGE_PASS_DOUBLE_THRESHOLD = 10  # Quality increases by 2 when ≤ this
    BACKSTAGE_PASS_TRIPLE_THRESHOLD = 5  # Quality increases by 3 when ≤ this

    def __init__(self, items):
        self.items = items


    def update_quality(self):
        for item in self.items:
            self.update_item_quality(item)
            self.update_sell_in(item)
            if item.sell_in < 0:
                self.handle_expired_item(item)

    def update_item_quality(self, item):
        if item.name == ITEM_NAME.AGED_BRIE:
            self.update_aged_brie(item)
        elif item.name == ITEM_NAME.BACKSTAGE_PASS:
            self.update_backstage_pass(item)
        elif ITEM_NAME.CONJURED in item.name:
            self.update_conjured_item(item)
        elif item.name != ITEM_NAME.SULFURAS:
            self.update_normal_item(item)

    def update_conjured_item(self, item):
        self.decrease_quality(item)
        self.decrease_quality(item)

    def update_sell_in(self, item):
        if item.name != ITEM_NAME.SULFURAS:
            item.sell_in -= 1

    def handle_expired_item(self, item):
        if item.name == ITEM_NAME.AGED_BRIE:
            self.increase_quality(item)
        elif item.name == ITEM_NAME.BACKSTAGE_PASS:
            item.quality = 0
        elif ITEM_NAME.CONJURED in item.name:
            self.decrease_quality(item)
            self.decrease_quality(item)
        elif item.name != ITEM_NAME.SULFURAS:
            self.decrease_quality(item)

    def update_aged_brie(self, item):
        self.increase_quality(item)

    def update_backstage_pass(self, item):
        self.increase_quality(item)
        if item.sell_in <= self.BACKSTAGE_PASS_DOUBLE_THRESHOLD:
            self.increase_quality(item)
        if item.sell_in <= self.BACKSTAGE_PASS_TRIPLE_THRESHOLD:
            self.increase_quality(item)

    def update_normal_item(self, item):
        self.decrease_quality(item)

    def increase_quality(self, item):
        if item.quality < self.MAX_QUALITY:
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

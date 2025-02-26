# -*- coding: utf-8 -*-

class GildedRose:
    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            if item.name == "Sulfuras, Hand of Ragnaros":
                continue  # Legendary item, no changes

            self.update_item(item)

    def update_item(self, item):
        if item.name == "Aged Brie":
            self.update_aged_brie(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            self.update_backstage_passes(item)
        else:
            self.update_regular_item(item)

        # Reduce sell_in for all items except "Sulfuras"
        item.sell_in -= 1

        # Handle expired items
        if item.sell_in < 0:
            self.handle_expired_item(item)

    def update_aged_brie(self, item):
        self.increase_quality(item)

    def update_backstage_passes(self, item):
        self.increase_quality(item)
        if item.sell_in < 11:
            self.increase_quality(item)
        if item.sell_in < 6:
            self.increase_quality(item)

    def update_regular_item(self, item):
        self.decrease_quality(item)

    def handle_expired_item(self, item):
        if item.name == "Aged Brie":
            self.increase_quality(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            item.quality = 0
        else:
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
        return f"{self.name}, {self.sell_in}, {self.quality}"
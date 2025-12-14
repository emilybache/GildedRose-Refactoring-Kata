# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:

            # Legendary item: no change
            if item.name == "Sulfuras, Hand of Ragnaros":
                continue

            # STEP 1 — Update quality before sell-in
            if item.name == "Aged Brie":
                self._increase_quality(item)

            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                self._update_backstage(item)

            elif "Conjured" in item.name:
                # Conjured before expiry = -2
                self._decrease_quality(item)
                self._decrease_quality(item)

            else:
                self._decrease_quality(item)

            # STEP 2 — decrease sell_in
            item.sell_in -= 1

            # STEP 3 — Handle expired items
            if item.sell_in < 0:

                if item.name == "Aged Brie":
                    self._increase_quality(item)

                elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                    item.quality = 0

                elif "Conjured" in item.name:
                    # Conjured after expiry = -4 total
                    self._decrease_quality(item)
                    self._decrease_quality(item)

                else:
                    # Normal items degrade -2 after expiry
                    self._decrease_quality(item)

    # --------------------------
    # Helper methods
    # --------------------------

    def _increase_quality(self, item):
        if item.quality < 50:
            item.quality += 1

    def _decrease_quality(self, item):
        if item.quality > 0:
            item.quality -= 1

    def _update_backstage(self, item):
        self._increase_quality(item)
        if item.sell_in < 11:
            self._increase_quality(item)
        if item.sell_in < 6:
            self._increase_quality(item)

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

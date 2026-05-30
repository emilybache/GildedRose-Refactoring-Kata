# -*- coding: utf-8 -*-
"""Gilded Rose inventory system.

The quality-update rules differ per item category. Rather than branch on the
item name inside one giant method, each category is modelled as an updater that
knows how to age a single item by one day. ``GildedRose`` simply dispatches each
item to the matching updater.

NOTE: The ``Item`` class and its attributes are owned by the "goblin in the
corner" and must not be modified, so all behaviour lives in the updaters.
"""

MIN_QUALITY = 0
MAX_QUALITY = 50

AGED_BRIE = "Aged Brie"
BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
SULFURAS = "Sulfuras, Hand of Ragnaros"
CONJURED_PREFIX = "Conjured"


def _clamp(quality):
    """Keep quality within the legal [0, 50] range."""
    return max(MIN_QUALITY, min(MAX_QUALITY, quality))


class ItemUpdater(object):
    """Base updater: a normal item degrades by 1, then by 2 once expired."""

    daily_degradation = 1

    def __init__(self, item):
        self.item = item

    def update(self):
        self._update_sell_in()
        self._update_quality()

    def _update_sell_in(self):
        self.item.sell_in -= 1

    def _update_quality(self):
        amount = self.daily_degradation
        if self.item.sell_in < 0:
            amount *= 2  # degrades twice as fast after the sell-by date
        self.item.quality = _clamp(self.item.quality - amount)


class AgedBrieUpdater(ItemUpdater):
    """Aged Brie increases in quality the older it gets, faster once expired."""

    def _update_quality(self):
        amount = 1
        if self.item.sell_in < 0:
            amount *= 2
        self.item.quality = _clamp(self.item.quality + amount)


class BackstagePassUpdater(ItemUpdater):
    """Backstage passes gain value as the concert approaches, then drop to zero
    once the concert has passed."""

    def _update_quality(self):
        if self.item.sell_in < 0:
            self.item.quality = 0
            return
        amount = 1
        if self.item.sell_in < 5:
            amount = 3
        elif self.item.sell_in < 10:
            amount = 2
        self.item.quality = _clamp(self.item.quality + amount)


class SulfurasUpdater(ItemUpdater):
    """Sulfuras is legendary: it never has to be sold and never changes."""

    def update(self):
        pass


class ConjuredUpdater(ItemUpdater):
    """Conjured items degrade twice as fast as normal items."""

    daily_degradation = 2

def updater_for(item):
    """Pick the right updater for an item based on its name."""
    if item.name == AGED_BRIE:
        return AgedBrieUpdater(item)
    if item.name == BACKSTAGE_PASS:
        return BackstagePassUpdater(item)
    if item.name == SULFURAS:
        return SulfurasUpdater(item)
    if item.name.startswith(CONJURED_PREFIX):
        return ConjuredUpdater(item)
    return ItemUpdater(item)

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            updater_for(item).update()


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)
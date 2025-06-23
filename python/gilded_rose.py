"""
    gilded_rose.py
    Refactoring logic for Gilded Rose Module.
    Author: Mohammed Mohideen M Z
"""

from dataclasses import dataclass
from abc import ABC, abstractmethod

@dataclass
class Item:
    """Represents an item with a name, sell_in, and quality."""
    name: str
    sell_in: int
    quality: int

    def __repr__(self):
        return f"{self.name}, {self.sell_in}, {self.quality}"


class ItemUpdater(ABC):
    """Abstract base class for updating item behavior."""
    def __init__(self, item):
        self.item = item

    def update(self):
        """Update the item's sell_in and quality."""
        pass


class NormalItem(ItemUpdater):
    """Standard item that degrades in quality as it gets older."""
    def update(self):
        self.item.sell_in -= 1
        if self.item.quality > 0:
            self.item.quality -= 1
        if self.item.sell_in < 0 and self.item.quality > 0:
            self.item.quality -= 1


class AgedBrie(ItemUpdater):
    """Item that increases in quality the older it gets."""
    def update(self):
        self.item.sell_in -= 1
        if self.item.quality < 50:
            self.item.quality += 1
            if self.item.sell_in < 0 and self.item.quality < 50:
                self.item.quality += 1


class BackstagePasses(ItemUpdater):
    """Item that increases in quality as its sell_in date approaches,
    but drops to 0 after the concert."""
    def update(self):
        self.item.sell_in -= 1
        if self.item.sell_in < 0:
            self.item.quality = 0
        else:
            if self.item.quality < 50:
                self.item.quality += 1
                if self.item.sell_in < 10:
                    self.item.quality += 1
                if self.item.sell_in < 5:
                    self.item.quality += 1
                self.item.quality = min(self.item.quality, 50)


class Sulfuras(ItemUpdater):
    """Legendary item that never changes in quality or sell_in."""


class ConjuredItem(ItemUpdater):
    """Item that degrades in quality twice as fast as normal items."""
    def update(self):
        self.item.sell_in -= 1
        degrade = 2
        if self.item.sell_in < 0:
            degrade *= 2
        self.item.quality = max(0, self.item.quality - degrade)


class GildedRose:
    """Main class that manages and updates a list of items."""
    def __init__(self, items):
        self.items = items

    @staticmethod
    def create_updater(item):
        if item.name == "Aged Brie":
            return AgedBrie(item)
        elif item.name == "Backstage passes":
            return BackstagePasses(item)
        elif item.name == "Sulfuras":
            return Sulfuras(item)
        elif "Conjured" in item.name:
            return ConjuredItem(item)
        else:
            return NormalItem(item)

    def update_quality(self):
        for item in self.items:
            updater = self.create_updater(item)
            updater.update()

# -*- coding: utf-8 -*-

"""
Gilded Rose Kata implementation.

This module defines the Item class, helper functions for quality and sell_in
management, and strategy classes for different item types. The GildedRose class
delegates item updates to the appropriate strategy.
"""


class Item:
    """
    Represents an inventory item.

    Attributes:
        name (str): The name of the item.
        sell_in (int): Number of days to sell the item.
        quality (int): Quality value of the item.
    """

    def __init__(self, name: str, sell_in: int, quality: int) -> None:
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self) -> str:
        return f"{self.name}, {self.sell_in}, {self.quality}"


# --- Helper functions ---

def clamp_quality(item: Item) -> None:
    """
    Ensure quality is within valid bounds (0–50), except Sulfuras which is fixed at 80.

    Args:
        item (Item): The item to clamp.
    """
    if item.name != "Sulfuras, Hand of Ragnaros":
        if item.quality < 0:
            item.quality = 0
        elif item.quality > 50:
            item.quality = 50


def increase_quality(item: Item, amount: int = 1) -> None:
    """
    Increase item quality by a given amount, respecting quality bounds.

    Args:
        item (Item): The item to update.
        amount (int): Amount to increase quality by (default 1).
    """
    item.quality += amount
    clamp_quality(item)


def decrease_quality(item: Item, amount: int = 1) -> None:
    """
    Decrease item quality by a given amount, respecting quality bounds.

    Args:
        item (Item): The item to update.
        amount (int): Amount to decrease quality by (default 1).
    """
    item.quality -= amount
    clamp_quality(item)


def decrease_sell_in(item: Item, amount: int = 1) -> None:
    """
    Decrease item sell_in by a given amount, except Sulfuras which never changes.

    Args:
        item (Item): The item to update.
        amount (int): Amount to decrease sell_in by (default 1).
    """
    if item.name != "Sulfuras, Hand of Ragnaros":
        item.sell_in -= amount


# --- Strategy classes ---

class ItemUpdater:
    """Abstract base class for item update strategies."""

    def update(self, item: Item) -> None:
        """Update the given item. Must be implemented by subclasses."""
        raise NotImplementedError


class SulfurasUpdater(ItemUpdater):
    """Updater for Sulfuras (legendary item)."""

    def update(self, item: Item) -> None:
        # Sulfuras never changes
        return


class AgedBrieUpdater(ItemUpdater):
    """Updater for Aged Brie (quality increases over time)."""

    def update(self, item: Item) -> None:
        decrease_sell_in(item, 1)
        if item.sell_in >= 0:
            increase_quality(item, 1)
        else:
            increase_quality(item, 2)


class BackstagePassUpdater(ItemUpdater):
    """Updater for Backstage passes (quality increases, then drops to 0 after concert)."""

    def update(self, item: Item) -> None:
        decrease_sell_in(item, 1)
        if item.sell_in < 0:
            item.quality = 0
        elif item.sell_in < 5:
            increase_quality(item, 3)
        elif item.sell_in < 10:
            increase_quality(item, 2)
        else:
            increase_quality(item, 1)


class NormalItemUpdater(ItemUpdater):
    """Updater for normal items (quality decreases over time)."""

    def update(self, item: Item) -> None:
        decrease_sell_in(item, 1)
        if item.sell_in >= 0:
            decrease_quality(item, 1)
        else:
            decrease_quality(item, 2)

#new requirement
class ConjuredItemUpdater(ItemUpdater):
    """Updater for Conjured items (degrade twice as fast)."""

    def update(self, item: Item) -> None:
        decrease_sell_in(item, 1)
        if item.sell_in >= 0:
            decrease_quality(item, 2)
        else:
            decrease_quality(item, 4)


# --- Factory ---

def get_updater(item: Item) -> ItemUpdater:
    """
    Return the appropriate updater strategy for the given item.

    Args:
        item (Item): The item to update.

    Returns:
        ItemUpdater: Strategy instance for the item type.
    """
    name = item.name
    if name == "Sulfuras, Hand of Ragnaros":
        return SulfurasUpdater()
    if name == "Aged Brie":
        return AgedBrieUpdater()
    if name == "Backstage passes to a TAFKAL80ETC concert":
        return BackstagePassUpdater()
    if "Conjured" in name:
        return ConjuredItemUpdater()
    return NormalItemUpdater()


# --- Main class ---

class GildedRose:
    """
    Main class for updating inventory items.

    Attributes:
        items (list[Item]): List of items to update.
    """

    def __init__(self, items: list[Item]) -> None:
        self.items = items

    def update_quality(self) -> None:
        """Update quality and sell_in for all items in inventory."""
        for item in self.items:
            updater = get_updater(item)
            updater.update(item)
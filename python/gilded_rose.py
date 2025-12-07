"""Gilded Rose Refactoring Kata."""
from __future__ import annotations


class GildedRose:
    """Manages inventory quality updates for the Gilded Rose inn.

    Updates item quality and sell_in values acording to specific business rules:
    - Normal items degrade in quality over time
    - "Aged Brie" increases in quality as it ages
    - "Sulfuras" is a legendary item that never changes
    - "Backstage passes" increase in quality as concert approaches, drop to 0 after
    - Quality is never negative and never exceeds 50 (except Sulfuras at 80)

    Goblin will throw a tantrum if the Item class is modified.
    """

    def __init__(self, items: list[Item]) -> None:
        """Initialize the GildedRose with a list of items.

        Parameters
        ----------
        items : list[Item]
            The list of items in the inventory.

        """
        self.items = items

    def update_quality(self) -> None:
        """Update quality and sell_in for all items according to the business rules."""
        for item in self.items:
            if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
                if item.quality > 0:
                    if item.name != "Sulfuras, Hand of Ragnaros":
                        item.quality = item.quality - 1
            else:
                if item.quality < 50:
                    item.quality = item.quality + 1
                    if item.name == "Backstage passes to a TAFKAL80ETC concert":
                        if item.sell_in < 11:
                            if item.quality < 50:
                                item.quality = item.quality + 1
                        if item.sell_in < 6:
                            if item.quality < 50:
                                item.quality = item.quality + 1
            if item.name != "Sulfuras, Hand of Ragnaros":
                item.sell_in = item.sell_in - 1
            if item.sell_in < 0:
                if item.name != "Aged Brie":
                    if item.name != "Backstage passes to a TAFKAL80ETC concert":
                        if item.quality > 0:
                            if item.name != "Sulfuras, Hand of Ragnaros":
                                item.quality = item.quality - 1
                    else:
                        item.quality = item.quality - item.quality
                else:
                    if item.quality < 50:
                        item.quality = item.quality + 1


class Item:
    """Represents an item in the Gilded Rose inventory.

    This class must not be modidied cuz Goblin said so.
    """

    def __init__(self, name: str, sell_in: int, quality: int) -> None:
        """Initialize an item in the Gilded Rose inventory.

        Parameters
        ----------
        name : str
            The item's name, which determines its update behavior.
        sell_in : int
            Number of days remaining to sell the item.
        quality : int
            Current quality value (0-50 for normal items, 80 for Sulfuras).

        """
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self) -> str:
        """Return a string representation of the item.

        Returns
        -------
        str
            A string describing the item with its name, sell_in, and quality.

        """
        return f"{self.name}, {self.sell_in}, {self.quality}"

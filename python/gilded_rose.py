# -*- coding: utf-8 -*-
from dataclasses import dataclass
from typing import Protocol


class UpdateStrategy(Protocol):
    """Interface for per-item-type quality update logic."""

    def update(self, item: "Item", days: int) -> None:
        """Mutate item.quality and item.sell_in to reflect `days` passing."""
        ...


class NormalStrategy:
    """
    Default degradation strategy.

    Quality decreases by 1 per day; by 2 per day once the sell date
    has passed (sell_in < 0). Quality never falls below 0.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = max(0, item.quality - 1)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = max(0, item.quality - 1)


class AgedBrieStrategy:
    """
    Quality improvement strategy for Aged Brie.

    Quality increases by 1 per day; by 2 per day once the sell date
    has passed. Quality never exceeds 50.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = min(50, item.quality + 1)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = min(50, item.quality + 1)


class BackstagePassStrategy:
    """
    Quality strategy for backstage passes.

    Quality increases as the concert approaches:
      - +1/day when more than 10 days remain
      - +2/day when 10 or fewer days remain
      - +3/day when 5 or fewer days remain
    Quality drops to 0 on concert day (sell_in == 0 at start of day)
    and stays 0 afterwards. Quality never exceeds 50.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            if item.sell_in <= 0:
                # Concert day or past — pass is worthless
                item.quality = 0
                item.sell_in -= 1
                continue
            if item.sell_in <= 5:
                item.quality = min(50, item.quality + 3)
            elif item.sell_in <= 10:
                item.quality = min(50, item.quality + 2)
            else:
                item.quality = min(50, item.quality + 1)
            item.sell_in -= 1


class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
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
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

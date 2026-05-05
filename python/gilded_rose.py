# -*- coding: utf-8 -*-
from __future__ import annotations

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


class SulfurasStrategy:
    """
    No-op strategy for Sulfuras, Hand of Ragnaros.

    Legendary items never degrade and never need to be sold.
    Neither quality nor sell_in is ever mutated.
    """

    def update(self, item: "Item", days: int) -> None:
        pass  # legendary items are immutable; parameters required by Protocol


class ConjuredStrategy:
    """
    Double-degradation strategy for Conjured items.

    Quality decreases by 2 per day; by 4 per day once the sell date
    has passed. Quality never falls below 0.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = max(0, item.quality - 2)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = max(0, item.quality - 2)


_STRATEGY_MAP: dict[str, UpdateStrategy] = {
    "Aged Brie": AgedBrieStrategy(),
    "Backstage passes to a TAFKAL80ETC concert": BackstagePassStrategy(),
    "Sulfuras, Hand of Ragnaros": SulfurasStrategy(),
    "Conjured Mana Cake": ConjuredStrategy(),
}


@dataclass
class ItemRecord:
    """
    Internal pairing of an Item with its UpdateStrategy.

    Built once in GildedRose.__init__ so the name-to-strategy lookup
    happens at construction time, not on every update call. Exposed
    publicly so tests can inject strategies directly without going
    through the name-lookup dict.
    """

    item: Item
    strategy: UpdateStrategy


class GildedRose:
    """
    Inventory manager for the Gilded Rose inn.

    Accepts a list of Item objects and updates their quality and sell_in
    values according to each item's type-specific strategy. The Item
    class and the items list are left untouched per the goblin's terms.

    Usage:
        rose = GildedRose(items)
        rose.update_quality()          # advance one day
        rose.update_quality(days=7)    # advance a full week
    """

    def __init__(self, items: list[Item]) -> None:
        self.items = items
        self._records: list[ItemRecord] = [
            ItemRecord(
                item=i,
                strategy=_STRATEGY_MAP.get(i.name, NormalStrategy()),
            )
            for i in items
        ]

    def update_quality(self, days: int = 1) -> None:
        """Advance quality and sell_in for all items by `days` days."""
        for record in self._records:
            record.strategy.update(record.item, days)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

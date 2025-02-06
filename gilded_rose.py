from dataclasses import dataclass


@dataclass
class Item:
    name: str
    sell_in: int
    quality: int

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


def update_item(item: Item):
    if should_do_nothing(item):
        return

    if is_backstage_pass(item):
        return update_backstage_pass(item)

    if is_aged_brie(item):
        if item.quality < 50:
            item.quality = item.quality + 1
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            if item.quality < 50:
                item.quality = item.quality + 1
        return

    if item.quality > 0:
        item.quality = item.quality - (1 if item.name != "Conjured" else 2)
    item.sell_in = item.sell_in - 1
    if item.sell_in < 0:
        if item.quality > 0:
            item.quality = item.quality - (1 if item.name != "Conjured" else 2)


class GildedRose:
    def update_quality(self, items: list[Item]):
        for item in items:
            update_item(item)


def is_aged_brie(item: Item):
    return item.name == "Aged Brie"


def should_do_nothing(item: Item):
    return item.name == "Sulfuras, Hand of Ragnaros"


def is_backstage_pass(item: Item):
    return item.name == "Backstage passes to a TAFKAL80ETC concert"


def update_backstage_pass(item: Item):
    if item.quality < 50:
        item.quality = item.quality + 1
        if item.sell_in < 11:
            if item.quality < 50:
                item.quality = item.quality + 1
        if item.sell_in < 6:
            if item.quality < 50:
                item.quality = item.quality + 1
    item.sell_in = item.sell_in - 1
    if item.sell_in < 0:
        item.quality = item.quality - item.quality
    return

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


def update_item(item: Item):
    if is_sulfuras(item):
        return

    if is_backstage_pass(item):
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

    if is_aged_brie(item):
        if item.quality < 50:
            item.quality = item.quality + 1
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            if item.quality < 50:
                item.quality = item.quality + 1
        return

    if item.quality > 0:
        item.quality = item.quality - 1
    item.sell_in = item.sell_in - 1
    if item.sell_in < 0:
        if item.quality > 0:
            item.quality = item.quality - 1


class GildedRose:
    def __init__(self, items: list[Item]):
        self.items = items

    def update_quality(self):
        for item in self.items:
            update_item(item)


def is_aged_brie(item: Item):
    return item.name == "Aged Brie"


def is_sulfuras(item: Item):
    return item.name == "Sulfuras, Hand of Ragnaros"


def is_backstage_pass(item: Item):
    return item.name == "Backstage passes to a TAFKAL80ETC concert"

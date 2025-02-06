class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


def is_aged_brie(item: Item) -> None:
    return item.name == "Aged Brie"


def update_item(item: Item):
    is_sulfuras = item.name == "Sulfuras, Hand of Ragnaros"
    is_backstage_pass = item.name == "Backstage passes to a TAFKAL80ETC concert"

    if is_sulfuras:
        return

    if is_aged_brie(item):
        if item.quality < 50:
            item.quality = item.quality + 1
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            if item.quality < 50:
                item.quality = item.quality + 1
        return

    if is_backstage_pass:
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

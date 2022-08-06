# -*- coding: utf-8 -*-


class GildedRose(object):
    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:

            if item.name == "Sulfuras, Hand of Ragnaros":
                # Sulfuras has quality 80, sell in value does not update
                if item.quality != 80:
                    raise ValueError(
                        "Sulfuras, Hand of Ragnaros has to have a quality equal 80."  # noqa: E501
                    )

            elif item.name == "Aged Brie":
                if item.sell_in > 0:
                    item.quality += 1
                else:
                    item.quality += 2
                item.quality = min(item.quality, 50)
                item.sell_in -= 1

            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                if item.sell_in <= 0:
                    item.quality = 0
                elif item.sell_in <= 5:
                    item.quality += 3
                elif item.sell_in <= 10:
                    item.quality += 2
                else:
                    item.quality += 1
                item.quality = min(item.quality, 50)
                item.sell_in -= 1

            elif "conjured" in item.name.lower():
                if item.sell_in > 0:
                    item.quality -= 2
                else:
                    item.quality -= 4
                item.quality = max(item.quality, 0)
                item.sell_in -= 1

            else:
                # Standard goods
                if item.sell_in > 0:
                    item.quality -= 1
                else:
                    item.quality -= 2
                item.sell_in -= 1
                item.quality = max(item.quality, 0)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

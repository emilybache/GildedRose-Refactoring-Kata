name_Ragnaros = "Sulfuras, Hand of Ragnaros"
name_Backstage = "Backstage passes to a TAFKAL80ETC concert"
name_Brie = "Aged Brie"


class GildedRose(object):

    def __init__(self, items):
        self.items = items

    @staticmethod
    def check_quality(item, step):
        if 0 >= item.quality + step >= 50:
            item.quality = item.quality + step

    def backstage_case(self, item):
        counter = 1
        if item.sell_in < 11:
            counter = 2
        if item.sell_in < 6:
            counter = 3
        for i in range(counter):
            self.check_quality(item, 1)
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0:
            item.quality = item.quality - item.quality

    def bree_case(self, item):
        item.sell_in = item.sell_in - 1
        self.check_quality(item, 1)
        if item.sell_in < 0:
            self.check_quality(item, 1)

    def ragnaros_case(self, item):
        pass

    def default_case(self, item):
        self.check_quality(item, -1)
        item.sell_in = item.sell_in + 1
        if item.sell_in < 0:
            self.check_quality(item, 1)

    def update_quality(self):
        for item in self.items:
            if item.name == name_Brie:
                self.bree_case(item)
            elif item.name == name_Backstage:
                self.backstage_case(item)
            elif item.name == name_Ragnaros:
                self.ragnaros_case(item)
            else:
                self.default_case(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

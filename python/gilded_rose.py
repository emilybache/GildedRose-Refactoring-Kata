# utf-8

class Updater(object):

    MIN_QUALITY = 0
    MAX_QUALITY = 50

    def normal_item(self, item):
        if item.sell_in > 0: 
            depreciation = -1
        else:
            depreciation = -2
        item.quality = max((item.quality + depreciation), self.MIN_QUALITY)
        item.sell_in += -1

    def aged_brie(self, item):
        if item.sell_in > 0:
            appreciation = 1
        else:
            appreciation = 2
        item.quality = min((item.quality + appreciation), self.MAX_QUALITY)
        item.sell_in += -1
    
    def sulfuras(self, item):
        pass

    def backstage_passes(self, item):

        def get_quality(item, appreciation):
            item.quality = min(item.quality + appreciation , self.MAX_QUALITY)

        if item.sell_in > 10:
            appreciation = 1
            get_quality(item, appreciation)
        elif item.sell_in > 5:
            appreciation = 2
            get_quality(item, appreciation)
        elif item.sell_in > 0:
            appreciation = 3
            get_quality(item, appreciation)
        else:
            item.quality = 0
        item.sell_in += -1


class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        updater = Updater()

        for item in self.items:
            if item.name == 'Aged Brie':
                updater.aged_brie(item)
            elif item.name == 'Backstage passes to a TAFKAL80ETC concert':
                updater.backstage_passes(item)
            elif item.name == 'Sulfuras, Hand of Ragnaros':
                updater.sulfuras(item)
            else:
                updater.normal_item(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

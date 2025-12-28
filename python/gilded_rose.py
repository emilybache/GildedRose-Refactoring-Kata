from typing import Final


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


class Updater(object):
    
    MIN_QUALITY:Final[int] = 0
    MAX_QUALITY: Final[int] = 50

    def normal_item(self, item:Item):
        if item.sell_in > 0: 
            depreciation = -1
        else:
            depreciation = -2
        item.quality = max((item.quality + depreciation), self.MIN_QUALITY)
        item.sell_in += -1

    def aged_brie(self, item:Item):
        if item.sell_in > 0:
            appreciation = 1
        else:
            appreciation = 2
        item.quality = min((item.quality + appreciation), self.MAX_QUALITY)
        item.sell_in += -1
    
    def sulfuras(self, item:Item):
        pass

    def backstage_passes(self, item:Item):

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

    def conjured(self, item:Item):
        if item.sell_in > 0:
            depreciation = -2
        else:
            depreciation = -4

        item.quality = max((item.quality + depreciation), self.MIN_QUALITY)
        item.sell_in += -1


class GildedRose(object):

    AGED_BRIE: Final[str] = "Aged Brie"
    BACKSTAGE_PASSES: Final[str] = "Backstage passes to a TAFKAL80ETC concert"
    SULFURAS: Final[str] = "Sulfuras, Hand of Ragnaros"
    CONJURED: Final[str] = "Conjured Mana Cake"

    def __init__(self, items:Item):
        self.items = items

    def update_quality(self):
        updater = Updater()

        for item in self.items:
            if item.name == self.AGED_BRIE:
                updater.aged_brie(item)
            elif item.name == self.BACKSTAGE_PASSES:
                updater.backstage_passes(item)
            elif item.name == self.SULFURAS:
                updater.sulfuras(item)
            elif item.name == self.CONJURED:
                updater.conjured(item)
            else:
                updater.normal_item(item)

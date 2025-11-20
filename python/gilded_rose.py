# -*- coding: utf-8 -*-

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
            for item in self.items:
                updater = self.run_the_logic_with_classes_safely(item)
                updater.update()        
                                        
    def run_the_logic_with_classes_safely(self, item):
        if item.name == "Aged Brie":
            return AgedBrieUpdater(item)
        elif item.name == "Sulfuras, Hand of Ragnaros":
            return SulfurasUpdater(item)
        elif item.name == "Backstage passes to a TAFKAL80ETC concert":
            return BackstagePassUpdater(item)
        elif item.name == "Conjured kitkats":
            return ConjuredItemUpdater(item)
        else:
            return ItemUpdater(item)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)



#__________________________________________ class to safely add items to the class item instead of if statements__________________________________________________________________________________

class ItemUpdater:
    def __init__(self, item):
        self.item = item

    def update(self):
        self.decrease_quality()
        self.decrease_sell_in()
        if self.item.sell_in < 0:
            self.decrease_quality()

    def decrease_quality(self):
        if self.item.quality > 0:
            self.item.quality -= 1

    def increase_quality(self):
        if self.item.quality < 50:
            self.item.quality += 1

    def decrease_sell_in(self):
        self.item.sell_in -= 1
        


#__________________________________________ classes to update the special cases in update quality function __________________________________________________________________________________


class AgedBrieUpdater(ItemUpdater):
    def update(self):
        self.increase_quality()
        self.decrease_sell_in()
        if self.item.sell_in < 0:
            self.increase_quality()

class SulfurasUpdater(ItemUpdater):
    def update(self):
        pass 

class BackstagePassUpdater(ItemUpdater):
    def update(self):
        self.increase_quality()
        if self.item.sell_in < 11:
            self.increase_quality()
        if self.item.sell_in < 6:
            self.increase_quality()
        
        self.decrease_sell_in()

        if self.item.sell_in < 0:
            self.item.quality = 0
            
#__________________________________________ class for the new item with a diffrent rule which is decrease twice as fast  __________________________________________________________________________________


class ConjuredItemUpdater(ItemUpdater):
    def decrease_quality(self):
        if self.item.quality > 0:
            self.item.quality -= 2  
        if self.item.quality < 0:
            self.item.quality = 0
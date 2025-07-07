# -*- coding: utf-8 -*-

"""
Refactoring the Gilded Rose kata
We use **polymorphism**: each item type has a specific `update_quality()` method, called uniformly.
This makes the system extensible (new item types can be added without changing existing code).
"""

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def update_quality(self):
        """
        Default behavior: 
        Quality decreases by 1 every day.
        and by 2 if the expiration date has passed.
        The Quality of an item is never negative.
        """
        self.sell_in -= 1
        if self.quality > 0:
            self.quality -= 1
        if self.sell_in < 0 and self.quality > 0:
            self.quality -= 1

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

# --------- Defining subclasses by Item using Polymorphism ---------

class AgedBrie(Item):
    """
    Aged Brie actually increases in Quality the older it gets. 
    The Quality of an item is never more than 50.
    """
    def update_quality(self):
        self.sell_in -= 1
        if self.quality >= 50:
            return
        self.quality += 1
        if self.sell_in < 0 and self.quality < 50:
            self.quality += 1


class Sulfuras(Item):
    """
    Sulfuras is a legendary item and as such its Quality is 80 and it never alters.
    Sulfuras, being a legendary item, never has to be sold or decreases in Quality.
    """
    def __init__(self, name, sell_in, quality):
        super().__init__(name, sell_in, 80)  # quality forcée à 80
    def update_quality(self):
        pass

class BackstagePasses(Item):
    def update_quality(self):
        """
        Backstage passes, like aged brie, increases in Quality as its SellIn value approaches;
        Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
        Quality drops to 0 after the concert
        """
        self.sell_in -= 1
        if self.sell_in < 0:
            self.quality = 0
            return

        if self.quality < 50:
            self.quality += 1
            if self.sell_in < 10 and self.quality < 50:
                self.quality += 1
            if self.sell_in < 5 and self.quality < 50:
                self.quality += 1

class Conjured(Item):
    def update_quality(self):
        """
        Conjured items degrade in Quality twice as fast as normal items
        """
        self.sell_in -= 1
        if self.quality > 0:
            self.quality -= 2
        if self.sell_in < 0 and self.quality > 0:
            self.quality -= 2


def generate_item(name, sell_in, quality):
    """
    returns the correct class based on the name of items.
    """
    if name == "Aged Brie":
        return AgedBrie(name, sell_in, quality)
    elif name == "Sulfuras, Hand of Ragnaros":
        return Sulfuras(name, sell_in, quality)
    elif name.startswith("Backstage passes"):
        return BackstagePasses(name, sell_in, quality)
    elif name.startswith("Conjured"):
        return Conjured(name, sell_in, quality)
    else:
        return Item(name, sell_in, quality)


class GildedRose:                  #In Python 3, it's not necessary to explicitly inherit from object.
    def __init__(self, items):
        self.items = items
        
    def update_quality(self):
        for item in self.items:
            item.update_quality()

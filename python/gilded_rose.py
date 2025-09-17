# -*- coding: utf-8 -*-

class Item:
    degradation_per_day: int = 1
    quality_min_value: int = 0
    quality_max_value: int = 50
    
    def __init__(self, name, sell_in, quality):
        assert quality >= self.quality_min_value
        assert quality <= self.quality_max_value
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __new__(cls, name, sell_in, quality):
        # If we are directly creating an Item, redirect to a mapped subclass
        if cls is Item and name in items_mappings:
            return super().__new__(items_mappings[name])
        return super().__new__(cls)
    
    def _update_quality(self, multiplier: int = 1):
        degradation = self.degradation_per_day * multiplier
        self.quality = max(self.quality_min_value, self.quality - degradation)
        self.quality = min(self.quality_max_value, self.quality)
    
    def _update_sell_in(self):
        self.sell_in -= 1
    
    def daily_step(self):
        if self.quality < 0:
            return

        # Decrement sell_in first, then apply quality changes based on new sell_in
        self._update_sell_in()
        if self.sell_in < 0:
            self._update_quality(2)
        else:
            self._update_quality()

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


class AgedBrie(Item):
    name: str = "Aged Brie"
    degradation_per_day: int = -1


class BackstagePasses(Item):
    name: str = "Backstage passes to a TAFKAL80ETC concert"
    def _update_quality(self, multiplier: int = 1):
        if self.sell_in <= 0:
            self.quality = 0
        elif 0 < self.sell_in < 6:
            self.quality += 3
        elif 6 < self.sell_in < 11:
            self.quality += 2
        else:
            self.quality += 1

        self.quality = min(50, self.quality)    
        


class Sulfuras(Item):
    name: str = "Sulfuras, Hand of Ragnaros"
    quality_max_value: int = 80
    quality_min_value: int = 80
    def __init__(self, name, sell_in, quality):
        super().__init__(name, 0, 80)

    def daily_step(self):
        # Legendary item: does not change in quality or sell_in
        pass


class Conjured(Item):
    name: str = "Conjured"
    degradation_per_day: int = Item.degradation_per_day * 2

items_mappings: dict[str, type[Item]] = {
    "Aged Brie": AgedBrie,
    "Backstage passes to a TAFKAL80ETC concert": BackstagePasses,
    "Sulfuras, Hand of Ragnaros": Sulfuras,
    "Conjured Mana Cake": Conjured,
    }

class GildedRose(object):
    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            item.daily_step()
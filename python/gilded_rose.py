# -*- coding: utf-8 -*-

class GildedRose(object):

    __AGED_BRIE = "Aged Brie"
    __BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
    __SULFURAS = "Sulfuras, Hand of Ragnaros"
    __CONJURED = "Conjured Mana Cake"
    

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self.update_item_quality(item)

    def update_item_quality(self, item):
        qualityIncrease = 1
        isExpired = item.sell_in < 0
        qualityDecrease = self.determine_degrate_quality_rate(item,isExpired)
        doesDegradeQaulity = item.name != GildedRose.__AGED_BRIE and item.name != GildedRose.__BACKSTAGE_PASSES and item.name != GildedRose.__SULFURAS
        
        if doesDegradeQaulity:
            self.adjust_quality(item, qualityDecrease)
        
        if item.name == GildedRose.__AGED_BRIE:
            qualityIncrease = 2 if isExpired else 1
            self.adjust_quality(item, qualityIncrease)
            
        if item.name == GildedRose.__BACKSTAGE_PASSES:
            self.update_backstagepasses_quality(item, isExpired, qualityIncrease)
        
        if item.name != GildedRose.__SULFURAS:
            item.sell_in = item.sell_in - 1

    # BAckstage Passes logic to update the item quality based on number of days
    def update_backstagepasses_quality(self, item, isExpired, qualityIncrease):
        self.adjust_quality(item, qualityIncrease)
        if item.sell_in < 11:
            self.adjust_quality(item, qualityIncrease)
        if item.sell_in < 6:
            self.adjust_quality(item, qualityIncrease)
        if isExpired:
            item.quality = item.quality - item.quality
        
    # Logic to determine the Quality Decrease rate based on Expired date and conjured item
    def determine_degrate_quality_rate(self, item, isExpired):
        qualityDecrease = -1 if item.name != GildedRose.__CONJURED else -2
        return qualityDecrease * 2 if isExpired else qualityDecrease


    # Update the quality item value if the quality value is within the range 0 to 50
    def adjust_quality(self, item, quality_increase_decrease):
        new_quality = item.quality + quality_increase_decrease
        if new_quality >= 0 and new_quality <= 50:
            item.quality = new_quality



class Item:  
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

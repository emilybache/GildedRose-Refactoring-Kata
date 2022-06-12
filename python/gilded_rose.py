# -*- coding: utf-8 -*-

class GildedRose(object):

    AGED_BRIE = "Aged Brie"
    BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
    SULFURAS = "Sulfuras, Hand of Ragnaros"
    CONJURED = "Conjured Mana Cake"
    qualityIncrease = 1

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            self.update_item_quality(item)

    def update_item_quality(self, item):
        isExpired = item.sell_in < 0
        qualityDecrease = self.determine_degrate_quality_rate(item,isExpired)
        doesDegradeQaulity = item.name != self.AGED_BRIE and item.name != self.BACKSTAGE_PASSES and item.name != self.SULFURAS
        
        if doesDegradeQaulity:
            self.adjust_quality(item, qualityDecrease)
        
        if item.name == self.AGED_BRIE:
            qualityIncrease = 2 if isExpired else 1
            self.adjust_quality(item, qualityIncrease)
            
        if item.name == self.BACKSTAGE_PASSES:
            self.adjust_quality(item, self.qualityIncrease)
            if item.sell_in < 11:
                self.adjust_quality(item, self.qualityIncrease)
            if item.sell_in < 6:
                self.adjust_quality(item, self.qualityIncrease)
            if isExpired:
                item.quality = item.quality - item.quality
        
        if item.name != self.SULFURAS:
            item.sell_in = item.sell_in - 1
        
    # Logic to determine the Quality Decrease rate based on Expired date and conjured item
    def determine_degrate_quality_rate(self, item, isExpired):
        qualityDecrease = -1 if item.name != self.CONJURED else -2
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

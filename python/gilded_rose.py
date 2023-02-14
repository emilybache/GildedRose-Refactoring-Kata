class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return f"{self.name}, {self.sell_in}, {self.quality}"

class RegularItem(Item):
    def update_quality(self):
        if self.quality > 0:
            self.quality -= 1
        if self.sell_in < 1:
            if self.quality > 0:
                self.quality -= 1
        self.sell_in -= 1

    
class AgedBrie(Item):
    def update_quality(self):
        if self.quality < 50:
            self.quality += 1
        if self.sell_in < 1:
            if self.quality < 50:
                self.quality += 1
        self.sell_in -= 1

class ElixirOfTheMongoose(Item):
    def update_quality(self):
        if self.quality > 0:
            self.quality -= 1
        if self.sell_in < 1:
            if self.quality > 0:
                self.quality -= 1
        self.sell_in -= 1

class Sulfuras(Item):
    def __init__(self, name, sell_in, quality):
        super().__init__(name, sell_in, quality)
        self.quality = 80
                
    def update_quality(self):
        pass

class BackstagePass(Item):
    def update_quality(self):
        if self.quality < 50:
            self.quality += 1
        if self.sell_in < 11:
            if self.quality < 50:
                self.quality += 1
        if self.sell_in < 6:
            if self.quality < 50:
                self.quality += 1
        if self.sell_in < 1:
            self.quality = 0
        self.sell_in -= 1

class ConjuredItem(Item):
    def update_quality(self):
        if self.quality > 0:
            self.quality -= 2
        if self.sell_in < 1:
            if self.quality > 0:
                self.quality -= 2
        self.sell_in -= 1
    
    
class GildedRose:
    def __init__(self, items: list):
        self.items = self._special_items(items)
        
    def update_quality(self):
        for item in self.items:
            item.update_quality()
    
    def _special_items(self, items):
        special_item_classes = {
            "Aged Brie": AgedBrie,
            "Elixir Of the Mongoose": ElixirOfTheMongoose,
            "Sulfuras, Hand of Ragnaros": Sulfuras,
            "Backstage passes": BackstagePass,
            "Conjured": ConjuredItem
        }
        
        corrected_list = [] 
        for item in items:
            special_item_class = self._get_special_item_class(item.name, special_item_classes)
            corrected_list.append(special_item_class(item.name, item.sell_in, item.quality))
                
        return corrected_list
    
    def _get_special_item_class(self, item_name, special_item_classes):
        for name, cls in special_item_classes.items():
            if name.lower() in item_name.lower() or item_name.lower() in name.lower():
                return cls
        return RegularItem
    
    def get_items(self):
        return self.items
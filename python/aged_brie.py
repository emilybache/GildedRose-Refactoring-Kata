from item_wrappers import ItemWrapper

class AgedBrieItem(ItemWrapper):
    def update(self):
        self.increase_quality()
        self.item.sell_in -= 1
        if self.item.sell_in < 0:
            self.increase_quality()

    def increase_quality(self):
        if self.item.quality < 50:
            self.item.quality += 1

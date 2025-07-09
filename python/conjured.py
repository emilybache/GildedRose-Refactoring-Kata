from item_wrappers import ItemWrapper

class ConjuredItem(ItemWrapper):
    def update(self):
        self.decrease_quality()
        self.decrease_quality()
        self.item.sell_in -= 1
        if self.item.sell_in < 0:
            self.decrease_quality()
            self.decrease_quality()

    def decrease_quality(self):
        if self.item.quality > 0:
            self.item.quality -= 1

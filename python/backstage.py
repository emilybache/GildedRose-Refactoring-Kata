from item_wrappers import ItemWrapper

class BackstagePassItem(ItemWrapper):
    def update(self):
        self.increase_quality()

        if self.item.sell_in <= 10:
            self.increase_quality()
        if self.item.sell_in <= 5:
            self.increase_quality()

        self.item.sell_in -= 1

        if self.item.sell_in < 0:
            self.item.quality = 0

    def increase_quality(self):
        if self.item.quality < 50:
            self.item.quality += 1

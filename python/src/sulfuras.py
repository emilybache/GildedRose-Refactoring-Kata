from python.components.gilded_rose import GildedRose, Item


class SulfurasLogic(GildedRose):
    def __init__(self, name, quality, sell_in):
        super().__init__(Item(name, quality, sell_in))

    def update_quality(self):
        # "Sulfuras" does not change in quality or sell_in
        return self.quality, self.sell_in
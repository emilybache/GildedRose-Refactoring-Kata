from python.components.gilded_rose import GildedRose, Item


class SulfurasLogic(GildedRose):
    def __init__(self, item: Item):
        super().__init__(item)

    def update_quality(self):
        # "Sulfuras" does not change in quality or sell_in
        return self.quality, self.sell_in
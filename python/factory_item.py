from python.item import AgedBrie, Sulfuras, Backstage, Conjured, RegularItem

item_to_class_map = {
    "Aged Brie": AgedBrie,
    "Sulfuras, Hand of Ragnaros": Sulfuras,
    "Backstage passes to a TAFKAL80ETC concert": Backstage,
    "Conjured": Conjured
}


class FactoryItem:

    @staticmethod
    def create_new_item(name, sell_in, quality):
        return item_to_class_map.get(name, RegularItem)(name, sell_in, quality)

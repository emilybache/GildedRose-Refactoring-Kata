from python.src.aged_brie import AgedBrieLogic
from python.src.backstage_passes import BackstagePassesLogic
from python.src.conjured import ConjuredItem
from python.src.normal_item_logic import NormalItemLogic
from python.src.sulfuras import SulfurasLogic
from python.models.items import Items
from python.components.gilded_rose import Item


class UpdateItemLogic:
    @staticmethod
    def update_items(items: list[Item]):
        quality, sell_in = None, None # initializing values
        for item in items:
            if item.name == item.aged_brie:
               quality, sell_in  = AgedBrieLogic(item).update_quality()
            elif item.name == Item.backstage_passes:
                quality, sell_in  = BackstagePassesLogic(item).update_quality()
            elif item.name == Item.sulfuras:
                quality, sell_in  = SulfurasLogic(item).update_quality()
            elif item.name == Item.conjured:
                quality, sell_in  = ConjuredItem(item).update_quality()
            else:
                quality, sell_in  = NormalItemLogic(item).update_quality()

        return quality, sell_in
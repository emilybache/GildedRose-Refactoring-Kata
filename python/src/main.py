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
        for item in items:
            if item.name == Items.aged_brie.value:
               quality, sell_in  = AgedBrieLogic(item).update_quality()
            elif item.name == Items.backstage_passes.value:
                quality, sell_in  = BackstagePassesLogic(item).update_quality()
            elif item.name == Items.sulfuras.value:
                quality, sell_in  = SulfurasLogic(item).update_quality()
            elif item.name == Items.conjured.value:
                quality, sell_in  = ConjuredItem(item).update_quality()
            else:
                quality, sell_in  = NormalItemLogic(item).update_quality()

        return quality, sell_in

# testing
#quality, sell_in = UpdateItemLogic().update_items([Item(name="Aged Brie", quality=10, sell_in=5)])
#quality, sell_in = UpdateItemLogic().update_items([Item(name="Backstage Passes", quality=10, sell_in=5)])
# quality, sell_in = UpdateItemLogic().update_items([Item(name="Conjured", quality=10, sell_in=5)])


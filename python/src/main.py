from python.src.aged_brie import AgedBrieLogic
from python.src.backstage_passes import BackstagePassesLogic
from python.src.normal_item_logic import NormalItemLogic
from python.src.sulfuras import SulfurasLogic


class UpdateItemLogic:
    @staticmethod
    def update_items(items):
        for item in items:
            if item.name == "Aged Brie":
                AgedBrieLogic.update_quality(item)
            elif item.name == "Backstage passes to a TAFKAL80ETC concert":
                BackstagePassesLogic.update_quality(item)
            elif item.name == "Sulfuras, Hand of Ragnaros":
                SulfurasLogic.update_quality(item)
            else:
                NormalItemLogic.update_quality(item)
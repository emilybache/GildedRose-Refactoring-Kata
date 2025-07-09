from models import Item

class ItemWrapper:
    """
    Classe abstraite représentant un item enrichi d'une logique métier propre.
    """
    def __init__(self, item: Item):
        self.item = item

    def update(self):
        raise NotImplementedError("Subclasses must implement update().")


class NormalItem(ItemWrapper):
    def update(self):
        self.decrease_quality()
        self.item.sell_in -= 1
        if self.item.sell_in < 0:
            self.decrease_quality()

    def decrease_quality(self):
        if self.item.quality > 0:
            self.item.quality -= 1


def item_factory(item: Item) -> ItemWrapper:
    """
    Retourne un wrapper métier adapté à l'item donné.
    """
    if item.name == "Aged Brie":
        from aged_brie import AgedBrieItem
        return AgedBrieItem(item)
    elif item.name == "Sulfuras, Hand of Ragnaros":
        from sulfuras import SulfurasItem
        return SulfurasItem(item)
    elif item.name == "Backstage passes to a TAFKAL80ETC concert":
        from backstage import BackstagePassItem
        return BackstagePassItem(item)
    elif "Conjured" in item.name:
        from conjured import ConjuredItem
        return ConjuredItem(item)
    else:
        return NormalItem(item)

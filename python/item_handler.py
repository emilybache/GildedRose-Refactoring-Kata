
class DefaultItemHandler(object):
    def _decrease_item_quality(self, item, amount: int = 1):
        item.quality = max(0, item.quality - amount)

    def _increase_item_quality(self, item, amount: int = 1):
        item.quality = min(50, item.quality + amount)

    def update_quality(self, item):
        self._decrease_item_quality(item)
        if item.sell_in < 0:
            self._decrease_item_quality(item)

    def update_sell_in(self, item):
        item.sell_in = item.sell_in - 1

class AgedBrieHandler(DefaultItemHandler):
    def update_quality(self, item):
        self._increase_item_quality(item)
        if item.sell_in < 0:
            self._increase_item_quality(item)

class BackstagePassesHandler(DefaultItemHandler):
    def update_quality(self, item):
        self._increase_item_quality(item)
        if item.sell_in < 10:
            self._increase_item_quality(item)
        if item.sell_in < 5:
            self._increase_item_quality(item)
        if item.sell_in < 0:
            item.quality = 0

class SulfurasHandler(DefaultItemHandler):
    def update_quality(self, item):
        pass

    def update_sell_in(self, item):
        pass

class ConjuredItemHandler(DefaultItemHandler):
    def update_quality(self, item):
        self._decrease_item_quality(item, 2)
        if item.sell_in < 0:
            self._decrease_item_quality(item, 2)

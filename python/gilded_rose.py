class GildedRose(object):

    def __init__(self, items):

        self.items = [self._create_specific_item(item) for item in items]

    def _create_specific_item(self, item):
        """
        :param item: Take param as Item object
        :return: Specified Item object based on specification.
        """
        if item.name.startswith("Aged Brie"):
            return AgedBrieItem(item.name, item.sell_in, item.quality)
        elif item.name.startswith("Sulfuras"):
            return SulfurasItem(item.name, item.sell_in, item.quality)
        elif item.name.startswith("Backstage"):
            return BackstagePassItem(item.name, item.sell_in, item.quality)
        elif item.name.startswith("Conjured"):
            return ConjuredItem(item.name, item.sell_in, item.quality)
        else:
            return NormalItem(item.name, item.sell_in, item.quality)

    def update_quality(self):
        for item in self.items:
            item.update_quality()


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

    def update_quality(self):

        self.sell_in -= 1
        if self.quality > 0:
            self.quality -= 1

        if self.sell_in < 0 and self.quality > 0:
            self.quality -= 1

    def _decrease_quality(self, amount=1):
        # Setting minimum quality to zero.
        self.quality = max(0, self.quality - amount)

    def _increase_quality(self, amount=1):
        # Setting a limit to 50 as described in spec
        self.quality = min(50, self.quality + amount)


class NormalItem(Item):
    pass


class AgedBrieItem(Item):
    def update_quality(self):
        self.sell_in -= 1
        self._increase_quality()  # It increases in quality as per spec
        if self.sell_in < 0:
            self._increase_quality()  # It increases in quality by double after sell_in passes as per spec


class SulfurasItem(Item):
    def update_quality(self):
        # It never changes any attributes
        pass


class BackstagePassItem(Item):
    def update_quality(self):
        self.sell_in -= 1
        self._increase_quality()

        if self.sell_in < 10:
            self._increase_quality()

        if self.sell_in < 5:
            self._increase_quality()

        if self.sell_in < 0:
            self.quality = 0


class ConjuredItem(Item):
    def update_quality(self):
        self.sell_in -= 1
        self._decrease_quality(2)  # It degrades twice as fast normal items
        if self.sell_in < 0:
            self._decrease_quality(2)  # It degrades twice as fast normal items

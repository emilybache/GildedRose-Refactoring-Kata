# -*- coding: utf-8 -*-

class GildedRose(object):
    MAX_NON_LEGENDARY_QUALITY = 50
    STATIC_LEGENDARY_QUALITY = 80
    STANDARD_INCREMENT = 1

    def __init__(self, items):
        self.items = items

    @staticmethod
    def _is_static_item(name: str) -> bool:
        static_items = [
            'Sulfuras, Hand of Ragnaros',
        ]
        return name in static_items

    @staticmethod
    def _is_inverse_quality_item(name: str) -> bool:
        inverse_quality_items = [
            'Aged Brie',
        ]
        return name in inverse_quality_items

    @staticmethod
    def _is_ranged_rule_based_item(name: str) -> bool:
        inverse_quality_items = [
            'Backstage passes to a TAFKAL80ETC concert',
        ]
        return name in inverse_quality_items

    @staticmethod
    def _is_double_increment_keyword_item(name: str) -> bool:
        ranged_rule_based_items = [
            'Conjured',
        ]
        for keyword in ranged_rule_based_items:
            if keyword in name:
                return True
            else:
                return False

    def _increase_quality(self, item):
        if item.quality < self.MAX_NON_LEGENDARY_QUALITY:
            item.quality += self.STANDARD_INCREMENT

    def _update_inverse_quality_item(self, item):
        self._increase_quality(item)
        item.sell_in -= self.STANDARD_INCREMENT
        if item.sell_in < 0:
            self._increase_quality(item)

    def _update_ranged_rule_based_item(self, item):
        if item.sell_in < 6:
            item.quality += self.STANDARD_INCREMENT * 3
        elif item.sell_in < 11:
            item.quality += self.STANDARD_INCREMENT * 2
        else:
            item.quality += self.STANDARD_INCREMENT
        item.sell_in -= self.STANDARD_INCREMENT
        if item.quality > self.MAX_NON_LEGENDARY_QUALITY:
            item.quality = self.MAX_NON_LEGENDARY_QUALITY
        if item.sell_in < 0:
            item.quality = 0

    def _update_item(self, item, increment):
        if item.quality > 0:
            item.quality -= increment
        item.sell_in -= self.STANDARD_INCREMENT
        if item.sell_in < 0 < item.quality:
            item.quality -= increment

    def update_quality(self):
        for item in self.items:
            # Catch static items
            if self._is_static_item(item.name):
                item.quality = self.STATIC_LEGENDARY_QUALITY
                continue
            # Catch items with inverse quality update
            elif self._is_inverse_quality_item(item.name):
                self._update_inverse_quality_item(item)
                continue
            # Catch items with range based rule
            elif self._is_ranged_rule_based_item(item.name):
                self._update_ranged_rule_based_item(item)
                continue
            # Catch conjured items
            elif self._is_double_increment_keyword_item(item.name):
                self._update_item(item, self.STANDARD_INCREMENT * 2)
                continue
            # Catch all other 'normal' items
            else:
                self._update_item(item, self.STANDARD_INCREMENT)


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

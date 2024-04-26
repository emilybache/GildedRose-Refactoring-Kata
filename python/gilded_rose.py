# -*- coding: utf-8 -*-

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)


class BaseRule:
    def __init__(self, degrade_rate: int = -1) -> None:
        self.degrade_rate = degrade_rate

    def _calculate_quality_diff(self, item: Item) -> int:
        sell_date_multiplier = 1 + int(item.sell_in < 0)
        return self.degrade_rate * sell_date_multiplier

    def update_quality(self, item: Item) -> None:
        calculated_new_quality = item.quality + self._calculate_quality_diff(item)
        # The quality of an item is never more than 50 and is never less than 0
        item.quality = min(50, max(0, calculated_new_quality))


class IncreasingQualityRule(BaseRule):
    def __init__(self) -> None:
        super().__init__(1)


class SulfurasRule(BaseRule):
    def update_quality(self, item: Item) -> None:
        # "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
        item.quality = 80


class BackstageRule(IncreasingQualityRule):
    def _calculate_quality_diff(self, item: Item) -> int:
        # If there are more than 10 days until the concert, same rules as parent class apply
        if item.sell_in > 10:
            return super()._calculate_quality_diff(item)
        # If there are 10 or fewer days until the concert, but more than 5, degrade_rate = 2
        if item.sell_in > 5:
            return 2
        # If there are 5 or fewer days until the concert, degrade_rate = 2
        return 3

    def update_quality(self, item: Item) -> None:
        # Tickets are useless when the concert is in the past
        if item.sell_in < 0:
            item.quality = 0
            return
        super().update_quality(item)


class ConjuredRule(BaseRule):
    def __init__(self):
        super().__init__(-2)


class RuleProvider:
    def __init__(self):
        self.base_rule = BaseRule()
        self.increasing_quality_rule = IncreasingQualityRule()
        self.sulfuras_rule = SulfurasRule()
        self.backstage_rule = BackstageRule()
        self.conjured_rule = ConjuredRule()

    def get_rule_for_item_name(self, name: str) -> BaseRule:
        if name.lower().startswith('sulfuras'):
            return self.sulfuras_rule
        if name.lower().startswith('aged'):
            return self.increasing_quality_rule
        if name.lower().startswith('backstage'):
            return self.backstage_rule
        if name.lower().startswith('conjured'):
            return self.conjured_rule
        return self.base_rule


class GildedRose(object):
    def __init__(self, items: list[Item]) -> None:
        self.items = items
        self.rule_provider = RuleProvider()

    def update_quality_over_range(self, update_range: int) -> None:
        for _ in range(update_range): self.update_quality()

    def update_quality(self) -> None:
        self.advance_day()
        for item in self.items: self.rule_provider.get_rule_for_item_name(item.name).update_quality(item)

    def advance_day(self) -> None:
        for item in self.items: item.sell_in -= 1

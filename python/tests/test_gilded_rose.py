"""Unit tests for Gilded Rose strategy classes."""
from __future__ import annotations

from gilded_rose import (
    AgedBrieStrategy,
    BackstagePassStrategy,
    ConjuredItemStrategy,
    Item,
    NormalItemStrategy,
    SulfurasStrategy,
)


class TestNormalItemStrategy:
    """Tests for normal item degradation."""

    def test_quality_decreases_by_one_before_sell_date(self) -> None:
        """Normal items lose 1 quality per day before sell date."""
        item = Item("+5 Dexterity Vest", sell_in=10, quality=20)
        strategy = NormalItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 19

    def test_quality_decreases_by_two_after_sell_date(self) -> None:
        """Normal items lose 2 quality per day after sell date."""
        item = Item("+5 Dexterity Vest", sell_in=0, quality=20)
        strategy = NormalItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 18

    def test_quality_never_negative(self) -> None:
        """Quality cannot go below zero."""
        item = Item("+5 Dexterity Vest", sell_in=5, quality=0)
        strategy = NormalItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 0

    def test_sell_in_decreases(self) -> None:
        """Sell_in decreases by 1 each day."""
        item = Item("+5 Dexterity Vest", sell_in=10, quality=20)
        strategy = NormalItemStrategy()

        strategy.update_sell_in(item)

        assert item.sell_in == 9


class TestAgedBrieStrategy:
    """Tests for Aged Brie appreciation."""

    def test_quality_increases_before_sell_date(self) -> None:
        """Aged Brie gains 1 quality per day before sell date."""
        item = Item("Aged Brie", sell_in=10, quality=0)
        strategy = AgedBrieStrategy()

        strategy.update_quality(item)

        assert item.quality == 1

    def test_quality_increases_twice_after_sell_date(self) -> None:
        """Aged Brie gains 2 quality per day after sell date."""
        item = Item("Aged Brie", sell_in=0, quality=0)
        strategy = AgedBrieStrategy()

        strategy.update_quality(item)

        assert item.quality == 2

    def test_quality_never_exceeds_50(self) -> None:
        """Quality cannot exceed 50."""
        item = Item("Aged Brie", sell_in=10, quality=50)
        strategy = AgedBrieStrategy()

        strategy.update_quality(item)

        assert item.quality == 50

    def test_sell_in_decreases(self) -> None:
        """Sell_in decreases by 1 each day."""
        item = Item("Aged Brie", sell_in=10, quality=0)
        strategy = AgedBrieStrategy()

        strategy.update_sell_in(item)

        assert item.sell_in == 9


class TestSulfurasStrategy:
    """Tests for Sulfuras legendary item."""

    def test_quality_never_changes(self) -> None:
        """Sulfuras quality remains constant at 80."""
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        strategy = SulfurasStrategy()

        strategy.update_quality(item)

        assert item.quality == 80

    def test_sell_in_never_changes(self) -> None:
        """Sulfuras never needs to be sold."""
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        strategy = SulfurasStrategy()

        strategy.update_sell_in(item)

        assert item.sell_in == 0


class TestBackstagePassStrategy:
    """Tests for Backstage pass appreciation."""

    def test_quality_increases_by_1_when_more_than_10_days(self) -> None:
        """Backstage pass gains 1 quality when > 10 days remain."""
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)
        strategy = BackstagePassStrategy()

        strategy.update_quality(item)

        assert item.quality == 21

    def test_quality_increases_by_2_when_10_days_or_less(self) -> None:
        """Backstage pass gains 2 quality when 6-10 days remain."""
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)
        strategy = BackstagePassStrategy()

        strategy.update_quality(item)

        assert item.quality == 22

    def test_quality_increases_by_3_when_5_days_or_less(self) -> None:
        """Backstage pass gains 3 quality when 1-5 days remain."""
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)
        strategy = BackstagePassStrategy()

        strategy.update_quality(item)

        assert item.quality == 23

    def test_quality_drops_to_zero_after_concert(self) -> None:
        """Backstage pass becomes worthless after concert."""
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=20)
        strategy = BackstagePassStrategy()

        strategy.update_quality(item)

        assert item.quality == 0

    def test_quality_never_exceeds_50(self) -> None:
        """Quality cannot exceed 50."""
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)
        strategy = BackstagePassStrategy()

        strategy.update_quality(item)

        assert item.quality == 50


class TestConjuredItemStrategy:
    """Tests for Conjured item degradation."""

    def test_quality_decreases_by_two_before_sell_date(self) -> None:
        """Conjured items lose 2 quality per day before sell date."""
        item = Item("Conjured Mana Cake", sell_in=10, quality=20)
        strategy = ConjuredItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 18

    def test_quality_decreases_by_four_after_sell_date(self) -> None:
        """Conjured items lose 4 quality per day after sell date."""
        item = Item("Conjured Mana Cake", sell_in=-1, quality=20)  # Past sell date
        strategy = ConjuredItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 16

    def test_quality_never_negative(self) -> None:
        """Quality cannot go below zero."""
        item = Item("Conjured Mana Cake", sell_in=5, quality=1)
        strategy = ConjuredItemStrategy()

        strategy.update_quality(item)

        assert item.quality == 0

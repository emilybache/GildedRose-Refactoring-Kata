# -*- coding: utf-8 -*-
import pytest
from gilded_rose import Item, GildedRose


class TestGildedRose:
    @pytest.mark.parametrize(
        "name, sell_in, quality, expected_sell_in, expected_quality",
        [
            ("Normal Item", 10, 20, 9, 19),
            ("Normal Item", 0, 10, -1, 8),
            ("Aged Brie", 2, 0, 1, 1),
            ("Aged Brie", -1, 49, -2, 50),
            ("Backstage passes", 11, 20, 10, 21),
            ("Backstage passes", 10, 20, 9, 22),
            ("Backstage passes", 5, 20, 4, 23),
            ("Backstage passes", 0, 20, -1, 0),
            ("Sulfuras", 0, 80, 0, 80),
            ("Conjured Mana Cake", 3, 6, 2, 4),
            ("Conjured Mana Cake", 0, 10, -1, 6),
        ]
    )
    def test_item_behavior(self, name, sell_in, quality, expected_sell_in, expected_quality):
        items = [Item(name, sell_in, quality)]
        gr = GildedRose(items)
        gr.update_quality()
        assert items[0].sell_in == expected_sell_in
        assert items[0].quality == expected_quality

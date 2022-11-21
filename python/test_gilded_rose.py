# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    # Once the sell by date has passed, Quality degrades twice as fast
    def test_quality_degrades_after_sell_by_date(self):
        items = [Item("foo", 1, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(9, items[0].quality) # Quality degraded by -1

        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(7, items[0].quality) # Quality degraded by -2

    # The Quality of an item is never negative
    def test_quality_never_negative(self):
        items = [Item("foo", 2, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(0, items[0].quality)

    # "Aged Brie" actually increases in Quality the older it gets
    def test_aged_brie_quality_increase_with_age(self):
        items = [Item("Aged Brie", 2, 1)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(2, items[0].quality)

    # The Quality of an item is never more than 50
    def test_quality_upper_limit(self):
        items = [Item("Aged Brie", 2, 49)] # Using Aged Brie as quality increases
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(50, items[0].quality)

        gilded_rose.update_quality()
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(50, items[0].quality)

    # "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
    def test_legendary_item(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 2, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(2, items[0].sell_in)
        self.assertEqual(80, items[0].quality)


    def test_backstage_passes(self):
        """
        "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
        Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
        Quality drops to 0 after the concert
        """
        # Ten days or less - quality increases by 2
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 9, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(12, items[0].quality)

        # Five days or less - quality increases by 3
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 3, 13)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(16, items[0].quality)

        # After the concert - quality drops to zero
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(0, items[0].quality)

    # "Conjured" items degrade in Quality twice as fast as normal items
    def test_conjured_degrade_twice_as_fast(self):
        items = [Item("Conjured Mana Cake", 1, 5)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()

        self.assertEqual(4, items[0].quality) # TODO

if __name__ == '__main__':
    unittest.main()

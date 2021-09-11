# -*- coding: utf-8 -*-
import unittest

import constants
from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("foo", items[0].name)

    def degradesTwiceAsFastAfterSellInDateHasPassed(self):
        items = [Item("foo", 0, 2)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(0, items[0].quality)

    def neverMustHaveNegativeQuality(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(0, items[0].quality)

    def increaseAgedBrieQualityWhenItGetsOlder(self):
        items = [Item(constants.AGED_BRIE, 0, 2)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(4, items[0].quality)

    def neverMustHaveAnItemWithMoreThan50OfQuality(self):
        items = [Item(constants.AGED_BRIE, 0, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)

    def neverModifySulfurasQuality(self):
        items = [Item(constants.SULFURAS, 0, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(50, items[0].quality)

    def neverModifySulfurasQualityEvenISGreaterThan50(self):
        items = [Item(constants.SULFURAS, 0, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(80, items[0].quality)

    def mustIncreaseBackstagePassesQualityWhenItsSellInApproaches(self):
        items = [
            Item(constants.BACKSTAGE, 15, 20),
            Item(constants.BACKSTAGE, 10, 20),
            Item(constants.BACKSTAGE, 5, 20),
        ]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEquals(21, items[0].quality)
        self.assertEquals(22, items[1].quality)
        self.assertEquals(23, items[2].quality)

    # def mustDecreaseQualityTwiceAsFastIfItemIsConjured(self):
    #     items = [Item(constants.CONJURED, 0, 20)]
    #     gilded_rose = GildedRose(items)
    #     gilded_rose.update_quality()
    #     self.assertEquals(16, items[0].quality)


if __name__ == "__main__":
    unittest.main()

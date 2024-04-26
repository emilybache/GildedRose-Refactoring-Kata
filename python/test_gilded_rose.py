# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_base_rule(self):
        items = [Item('+5 Dexterity Vest', 16, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(15, items[0].sell_in)
        self.assertEqual(49, items[0].quality)

        gilded_rose.update_quality_over_range(15)
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(34, items[0].quality)

        gilded_rose.update_quality()
        self.assertEqual(15, items[0].sell_in)
        self.assertEqual(49, items[0].quality)

    def test_backstage_rule(self):
        items = [Item('Backstage passes to a TAFKAL80ETC concert', 16, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(15, items[0].sell_in)
        self.assertEqual(1, items[0].quality)

        gilded_rose.update_quality_over_range(4)
        self.assertEqual(11, items[0].sell_in)
        self.assertEqual(5, items[0].quality)

        gilded_rose.update_quality()
        self.assertEqual(10, items[0].sell_in)
        self.assertEqual(7, items[0].quality)

        gilded_rose.update_quality_over_range(4)
        self.assertEqual(6, items[0].sell_in)
        self.assertEqual(15, items[0].quality)

        gilded_rose.update_quality()
        self.assertEqual(5, items[0].sell_in)
        self.assertEqual(18, items[0].quality)

        gilded_rose.update_quality_over_range(5)
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(33, items[0].quality)

        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)


if __name__ == '__main__':
    unittest.main()

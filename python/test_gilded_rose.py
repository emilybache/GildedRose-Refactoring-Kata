# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_normal_item_quality(self):
        item = Item(name="foo", sell_in=5, quality=5)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(4, item.quality)

    def test_normal_item_quality_when_quality_is_zero(self):
        item = Item(name="foo", sell_in=0, quality=0)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        # Quality should never go below 0
        self.assertEqual(0, item.quality)

    def test_normal_item_quality_if_sell_in_is_0(self):
        item = Item(name="foo", sell_in=0, quality=4)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        # Quality will degrade by 2 once sell in date passed
        self.assertEqual(2, item.quality)

    def test_aged_brie_quality_exceeded(self):
        item = Item(name="Aged Brie", sell_in=0, quality=50)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        # Quality should never exceed fifty
        self.assertEqual(50, item.quality)

    def test_aged_brie_sell_in(self):
        item = Item(name="Aged Brie", sell_in=10, quality=2)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEquals(9, item.sell_in)

    def test_sulfuras_sell_in_and_quality(self):
        item = Item(name="Sulfuras, Hand of Ragnaros", sell_in=10, quality=2)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        # For sulfuras, sell in and quality both remains same
        self.assertEquals(10, item.sell_in)
        self.assertEquals(2, item.quality)

    def test_backstage_quality_for_less_than_ten_days(self):
        item = Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=9, quality=2)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEquals(8, item.sell_in)

        # For backstage, quality increase by 2 when sell_in <=10, and > 5
        self.assertEquals(4, item.quality)

    def test_backstage_quality_for_less_than_five_days(self):
        item = Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=2)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEquals(4, item.sell_in)

        # For backstage, quality increase by 3 when sell_in <=5, and > 0
        self.assertEquals(5, item.quality)

    def test_backstage_quality_when_concert_is_over(self):
        item = Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=2)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEquals(-1, item.sell_in)

        # For backstage, quality gets to 0 when concert is over that is sell_in <= 0
        self.assertEquals(0, item.quality)

        
if __name__ == '__main__':
    unittest.main()

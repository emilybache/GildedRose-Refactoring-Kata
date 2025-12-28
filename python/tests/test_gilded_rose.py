import unittest
from gilded_rose import GildedRose, Item


class GildedRoseTest(unittest.TestCase):


    def updater(obj = GildedRose, times = 1):
        for i in range(times):
            obj.update_quality()

    def test_normal_item(self):
        # Set up the item
        item = Item("+5 Dexterity Vest", 10, 20)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 19)
        
        # Sell by date passed
        GildedRoseTest.updater(gilded_rose, item.sell_in + 1)
        self.assertEqual(item.sell_in, -1)

        # Quality degrades twice as fast now
        self.assertEqual(item.quality, 8)
        GildedRoseTest.updater(gilded_rose, 10)
        self.assertEqual(item.sell_in, -11)
        # never be less than 0
        self.assertEqual(item.quality, 0)  

    def test_aged_brie(self):
        # Set up the item
        item = Item("Aged Brie", 10, 20)
        gilded_rose = GildedRose([item])
        
        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 21)

        # Sell by date passed
        GildedRoseTest.updater(gilded_rose, item.sell_in + 1)
        self.assertEqual(item.sell_in, -1)

        # Quality increases twice as fast now
        self.assertEqual(item.quality, 32)
        GildedRoseTest.updater(gilded_rose, 10)
        self.assertEqual(item.sell_in, -11)
        self.assertEqual(item.quality, 50) 

    def test_sulfuras(self):
        # Set up the item
        item = Item("Sulfuras, Hand of Ragnaros", 20, 80)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertNotEqual(item.sell_in, 19)
        self.assertEqual(item.sell_in, 20)
        self.assertEqual(item.quality, 80)
        
        # sell by date passed
        GildedRoseTest.updater(gilded_rose, item.sell_in + 1)
        self.assertEqual(item.sell_in, 20)
        self.assertNotEqual(item.sell_in, -1)

    def test_backstage_passes(self):
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", 20, 20)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 19)
        self.assertEqual(item.quality, 21)
        GildedRoseTest.updater(gilded_rose, 9)

        # Sell in 10 or less
        self.assertEqual(item.sell_in, 10)
        self.assertEqual(item.quality, 30)
        GildedRoseTest.updater(gilded_rose)
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 32)

        # Sell in 5 or less
        GildedRoseTest.updater(gilded_rose, 4)
        self.assertEqual(item.sell_in, 5)
        self.assertEqual(item.quality, 40)
        GildedRoseTest.updater(gilded_rose)
        self.assertEqual(item.sell_in, 4)
        self.assertEqual(item.quality, 43)
        GildedRoseTest.updater(gilded_rose, 2)
        self.assertEqual(item.sell_in, 2)
        self.assertEqual(item.quality, 49)

        # Never gets beyond 50 and drops to 0 once sell in passed
        GildedRoseTest.updater(gilded_rose)
        self.assertEqual(item.sell_in, 1)
        self.assertEqual(item.quality, 50)

        GildedRoseTest.updater(gilded_rose, 2)
        self.assertEqual(item.sell_in, -1)
        self.assertEqual(item.quality, 0)

if __name__ == '__main__':
    unittest.main()

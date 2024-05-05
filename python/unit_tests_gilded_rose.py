import unittest
from gilded_rose import GildedRose, Item


class GildedRoseTest(unittest.TestCase):

    def test_normal_item(self):
        # Set up the item
        item = Item("Classic Item", 10, 20)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 19)

        # Sell by date passed
        # Set up the item
        item = Item("Classic Item", -1, 10)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -2)
        self.assertEqual(item.quality, 8)

        # Quality degrades twice as fast now
        # Set up the item
        item = Item("+5 Dexterity Vest", -1, 0)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -2)
        self.assertEqual(item.quality, 0)  # never be less than 0

    def test_aged_brie(self):
        # Set up the item
        item = Item("Aged Brie", 10, 20)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 21)

        # Sell by date passed
        # Set up the item
        item = Item("Aged Brie", -1, 10)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -2)
        self.assertEqual(item.quality, 12)

        # Quality increases twice as fast now
        # Set up the item
        item = Item("Aged Brie", -1, 50)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -2)
        self.assertEqual(item.quality, 50)  # never be more than 50

    def test_sulfuras(self):
        # Set up the item
        item = Item("Sulfuras, Hand of Ragnaros", 20, 80)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertNotEqual(item.sell_in, 19)
        self.assertEqual(item.sell_in, 20)
        self.assertEqual(item.quality, 80)

        # Sell by date passed
        # Set up the item
        item = Item("Sulfuras, Hand of Ragnaros", -1, 10)
        gilded_rose = GildedRose([item])

        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, -1)
        self.assertEqual(item.quality, 10)

    def test_backstage_passes(self):
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", 20, 20)
        gilded_rose = GildedRose([item])

        # One day after
        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 19)
        self.assertEqual(item.quality, 21)

        # Sell in 10 or less
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)
        gilded_rose = GildedRose([item])

        gilded_rose.update_quality()
        self.assertEqual(item.sell_in, 9)
        self.assertEqual(item.quality, 22)

        # Sell in 5 or less
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", 4, 20)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        self.assertEqual(item.sell_in, 3)
        self.assertEqual(item.quality, 23)

        # Never gets beyond 50 and drops to 0 once sell in passed
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", 4, 50)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        self.assertEqual(item.sell_in, 3)
        self.assertEqual(item.quality, 50)

        # Drops to 0 once sell in passed
        # Set up the item
        item = Item("Backstage passes to a TAFKAL80ETC concert", -1, 50)
        gilded_rose = GildedRose([item])
        gilded_rose.update_quality()

        self.assertEqual(item.sell_in, -2)
        self.assertEqual(item.quality, 0)


if __name__ == "__main__":
    unittest.main()

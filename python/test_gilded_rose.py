
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):

    def test_regular_item(self):
        items = [Item("Normal Item", 10, 15)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(items[0].days_left, 9) #days decreases
        self.assertEqual(items[0].quality, 14) #quality decreases

    def test_aged_brie(self):
        items = [Item("Aged Brie", 10, 40)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(items[0].quality,41)  #increases in quality

    def test_backstage_passes(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(items[0].quality, 23)  #increases in quality by 3

    def test_sulfuras(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 5, 80)]
        gr = GildedRose(items)
        gr.update_quality()
        self.assertEqual(items[0].quality, 80) #stays the same
        self.assertEqual(items[0].days_left, 5)  #stays the same



if __name__ == '__main__':
    unittest.main()


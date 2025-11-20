import unittest
from gilded_rose import Item, GildedRose

class GildedRoseTest(unittest.TestCase):
    
    def test_normal_item_before_sell_date(self):
        items = [Item("Any normal Item ", 10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(19, items[0].quality)

    def test_normal_item_on_sell_date(self):
        items = [Item("Any normal Item ", 0, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(18, items[0].quality)

    def test_normal_item_after_sell_date(self):
        items = [Item("Any normal Item ", -10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-11, items[0].sell_in)
        self.assertEqual(18, items[0].quality)

    def test_normal_item_quality_zero(self):
        items = [Item("Any normal Item ", 10, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)

    def test_aged_brie_before_sell_date(self):
        items = [Item("Aged Brie", 10, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(11, items[0].quality)

    def test_aged_brie_after_sell_date(self):
        items = [Item("Aged Brie", -5, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(12, items[0].quality)

    def test_aged_brie_max_quality(self):
        items = [Item("Aged Brie", 10, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)

    def test_sulfuras_never_changes(self):
        items = [Item("Sulfuras, Hand of Ragnaros", 10, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(10, items[0].sell_in)
        self.assertEqual(80, items[0].quality)

    def test_backstage_pass_long_before_sell_date(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(21, items[0].quality)

    def test_backstage_pass_close_to_sell_date(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(22, items[0].quality)

    def test_backstage_pass_very_close_to_sell_date(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(23, items[0].quality)

    def test_backstage_pass_after_sell_date(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
        
    def test_conjured_item_degrades_twice_as_fast(self):
        items = [Item("Conjured kitkats", 10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(18, items[0].quality)

if __name__ == '__main__':
    unittest.main()
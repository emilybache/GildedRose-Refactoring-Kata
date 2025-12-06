# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    
    def test_normal_item_degradation(self):
        """Normal items decrease quality by 1 each day"""
        items = [Item("+5 Dexterity Vest", 10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(19, items[0].quality)
    
    def test_normal_item_quality_never_negative(self):
        """Quality never goes below 0"""
        items = [Item("Normal Item", 5, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
    
    def test_normal_item_after_sell_by_date(self):
        """Normal items degrade twice as fast after sell by date"""
        items = [Item("Normal Item", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(8, items[0].quality)  # Degrades by 2 (1 before, 1 after)
    
    def test_aged_brie_increases_quality(self):
        """Aged Brie increases in quality as it gets older"""
        items = [Item("Aged Brie", 2, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(1, items[0].sell_in)
        self.assertEqual(1, items[0].quality)
    
    def test_aged_brie_quality_cap(self):
        """Aged Brie quality never exceeds 50"""
        items = [Item("Aged Brie", 2, 50)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)
    
    def test_aged_brie_after_sell_by_date(self):
        """Aged Brie increases quality by 2 after sell by date"""
        items = [Item("Aged Brie", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(12, items[0].quality)  # +1 normal, +1 after sell by
    
    def test_sulfuras_never_changes(self):
        """Sulfuras never has to be sold and never decreases in quality"""
        items = [Item("Sulfuras, Hand of Ragnaros", 0, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].sell_in)
        self.assertEqual(80, items[0].quality)
    
    def test_sulfuras_negative_sell_in(self):
        """Sulfuras maintains quality even with negative sell_in"""
        items = [Item("Sulfuras, Hand of Ragnaros", -1, 80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(80, items[0].quality)
    
    def test_backstage_passes_more_than_10_days(self):
        """Backstage passes increase quality by 1 when more than 10 days left"""
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(14, items[0].sell_in)
        self.assertEqual(21, items[0].quality)
    
    def test_backstage_passes_10_days_or_less(self):
        """Backstage passes increase quality by 2 when 10 days or less"""
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(9, items[0].sell_in)
        self.assertEqual(22, items[0].quality)  # +1 base, +1 for <=10 days
    
    def test_backstage_passes_5_days_or_less(self):
        """Backstage passes increase quality by 3 when 5 days or less"""
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(4, items[0].sell_in)
        self.assertEqual(23, items[0].quality)  # +1 base, +1 for <=10, +1 for <=5
    
    def test_backstage_passes_quality_cap(self):
        """Backstage passes quality never exceeds 50"""
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(50, items[0].quality)
    
    def test_backstage_passes_after_concert(self):
        """Backstage passes quality drops to 0 after the concert"""
        items = [Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(0, items[0].quality)
    
    def test_conjured_item_degradation(self):
        """Conjured items degrade in quality twice as fast as normal items"""
        items = [Item("Conjured Mana Cake", 3, 6)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(2, items[0].sell_in)
        self.assertEqual(4, items[0].quality)  # Degrades by 2 instead of 1
    
    def test_conjured_item_after_sell_by_date(self):
        """Conjured items degrade twice as fast after sell by date (4x total)"""
        items = [Item("Conjured Mana Cake", 0, 10)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(-1, items[0].sell_in)
        self.assertEqual(6, items[0].quality)  # -2 normal, -2 after sell by = -4 total
    
    def test_conjured_item_quality_never_negative(self):
        """Conjured items quality never goes below 0"""
        items = [Item("Conjured Mana Cake", 3, 1)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)  # Would be -1, but capped at 0
    
    def test_conjured_item_quality_never_negative_after_sell_by(self):
        """Conjured items quality never goes below 0 even after sell by date"""
        items = [Item("Conjured Mana Cake", 0, 1)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)  # Would be -3, but capped at 0
    
    def test_multiple_items(self):
        """Test that multiple items are updated correctly"""
        items = [
            Item("Normal Item", 5, 10),
            Item("Aged Brie", 5, 10),
            Item("Sulfuras, Hand of Ragnaros", 5, 80),
            Item("Backstage passes to a TAFKAL80ETC concert", 5, 10),
            Item("Conjured Mana Cake", 5, 10)
        ]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        
        self.assertEqual(9, items[0].quality)  # Normal: -1
        self.assertEqual(11, items[1].quality)  # Aged Brie: +1
        self.assertEqual(80, items[2].quality)  # Sulfuras: unchanged
        self.assertEqual(13, items[3].quality)  # Backstage: +3
        self.assertEqual(8, items[4].quality)  # Conjured: -2

        
if __name__ == '__main__':
    unittest.main()

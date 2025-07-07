# -*- coding: utf-8 -*-
import unittest

from gilded_rose import Item, GildedRose, AgedBrie, Sulfuras, BackstagePasses, Conjured


class GildedRoseTest(unittest.TestCase):
    def test_foo(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual("foo", items[0].name)

# ---------------- tests Aged Brie -------------------
def test_aged_brie_quality_increase_one_before_sellin_decrease():
    item = AgedBrie(name="Aged Brie", sell_in=2, quality=0)
    item.update_quality()
    assert item.sell_in == 1
    assert item.quality == 1

def test_aged_brie_quality_increase_2_before_sellin_under_0():
    item = AgedBrie(name="Aged Brie", sell_in=0, quality=1)
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 3


def test_aged_brie_quality_never_more_50():
    """
    The Quality of an item is never more than 50.
    """
    item = AgedBrie(name="Aged Brie", sell_in=-15, quality=49)
    item.update_quality()
    assert item.sell_in == -16
    assert item.quality == 50

def test_aged_brie_failed_test():
    item = AgedBrie(name="Aged Brie", sell_in=0, quality=1)
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 2

# ---------------- tests Sulfuras -------------------

def test_sulfuras_quality_is_80_and_it_never_change():
    """
    Sulfuras is a legendary item and as such its Quality is 80 and it never alters.
    """
    item = Sulfuras(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=85)                                                         
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 80

def test_sulfuras_quality_is_forced_to_80():
    """
    Sulfuras, being a legendary item, never has to be sold or decreases in Quality.
    """
    item = Sulfuras(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)                                                         
    item.update_quality()
    assert item.sell_in == 0
    assert item.quality == 80


#  ---------------- tests BackstagePasses -------------------

def test_backstage_passes_quality_increase_one_before_sellin_decrease():
    """
    Backstage passes, like aged brie, increases in Quality as its SellIn value approaches
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)                                                        
    item.update_quality()
    assert item.sell_in == 14
    assert item.quality == 21

def test_backstage_passes_quality_increase_2_when_there_are_10_days_or_less():
    """
    Quality increases by 2 when there are 10 days or less.
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=44)                                                        
    item.update_quality()
    assert item.sell_in == 9
    assert item.quality == 46

def test_backstage_passes_quality_increase_2_when_there_are_10_days_or_less():
    """
    Quality increases by by 3 when there are 5 days or less but.
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=4, quality=43)                                                        
    item.update_quality()
    assert item.sell_in == 3
    assert item.quality == 46

def test_backstage_passes_quality_drops_to_0_after_the_concert():
    """
    Quality drops to 0 after the concert
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=49)                                                       
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 0

def test_backstage_passes_quality_never_more_50():
    """
    The Quality of an item is never more than 50
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=49)                                                       
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 0

def test_backstage_passes_quality_never_more_50():
    """
    The Quality of an item is never more than 50.
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=3, quality=49)
    item.update_quality()
    assert item.sell_in == 2
    assert item.quality == 50

def test_backstage_passes_failed_test():
    """
    The Quality of an item is never more than 50.
    """
    item = BackstagePasses(name="Backstage passes to a TAFKAL80ETC concert", sell_in=3, quality=49)
    item.update_quality()
    assert item.sell_in == 2
    assert item.quality == 52
        
#  ---------------- tests Conjured -------------------

def test_conjured_degrade_in_quality_twice_as_fast_as_normal_items():
    """
    Conjured items degrade in Quality twice as fast as normal items
    """
    item = Conjured(name="Conjured Mana Cake", sell_in=3, quality=6)
    item.update_quality()
    assert item.sell_in == 2
    assert item.quality == 4

def test_conjured_failed_test():
    """
    Conjured items degrade in Quality one
    """
    item = Conjured(name="Conjured Mana Cake", sell_in=0, quality=4)
    item.update_quality()
    assert item.sell_in == -1
    assert item.quality == 3

if __name__ == '__main__':
    unittest.main()


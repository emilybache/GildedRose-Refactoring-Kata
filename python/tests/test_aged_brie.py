from python.components.gilded_rose import Item
import unittest
from python.src.aged_brie import AgedBrieLogic


class TestAgedBrieLogic(unittest.TestCase):

    def test_update_quality_before_sell_in(self):
        # Test case where sell_in is greater than 0
        item = Item(name="Aged Brie",sell_in=5,quality=10)
        aged_brie = AgedBrieLogic(item)
        quality, sell_in = aged_brie.update_quality()
        self.assertEqual(quality, 11)  # Quality increases by 1
        self.assertEqual(sell_in, 4)  # Sell_in decreases by 1

    def test_update_quality_after_sell_in(self):
        # Test case where sell_in is 0 or negative
        item = Item(name="Aged Brie", sell_in= 0, quality=10)
        aged_brie = AgedBrieLogic(item)
        quality, sell_in = aged_brie.update_quality()
        self.assertEqual(quality, 12)  # Quality increases by 2 after sell_in is 0
        self.assertEqual(sell_in, -1)  # Sell_in decreases by 1

    def test_update_quality_max_quality(self):
        # Test case where quality is already at 50
        item = Item(name="Aged Brie", sell_in=5, quality=50)
        aged_brie = AgedBrieLogic(item)
        quality, sell_in = aged_brie.update_quality()
        self.assertEqual(quality, 50)  # Quality should not increase above 50
        self.assertEqual(sell_in, 4)  # Sell_in decreases by 1

    def test_update_quality_sell_in_negative(self):
        # Test case where sell_in is negative and quality is not at max
        item = Item(name="Aged Brie", sell_in=-1, quality=48)
        aged_brie = AgedBrieLogic(item)
        quality, sell_in = aged_brie.update_quality()
        self.assertEqual(quality, 50)  # Quality increases by 2 when sell_in is negative
        self.assertEqual(sell_in, -2)  # Sell_in decreases by 1


if __name__ == "__main__":
    unittest.main()
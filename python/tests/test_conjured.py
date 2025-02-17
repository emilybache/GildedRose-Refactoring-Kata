import unittest
from python.components.gilded_rose import Item
from python.src.conjured import ConjuredItem


class TestConjuredItem(unittest.TestCase):

    def test_update_quality_before_sell_in(self):
        # Test case where sell_in > 0
        item = Item(name="Conjured", sell_in=5, quality=10)
        conjured_item = ConjuredItem(item)
        quality, sell_in = conjured_item.update_quality()
        self.assertEqual(quality, 8)  # Quality decreases by 2
        self.assertEqual(sell_in, 4)  # Sell_in decreases by 1

    def test_update_quality_after_sell_in(self):
        # Test case where sell_in <= 0
        item = Item(name="Conjured", sell_in=0, quality=10)
        conjured_item = ConjuredItem(item)
        quality, sell_in = conjured_item.update_quality()
        self.assertEqual(quality, 6)  # Quality decreases by 2 twice (once for regular update and once after sell_in <= 0)
        self.assertEqual(sell_in, -1)  # Sell_in decreases by 1

    def test_update_quality_when_sell_in_is_zero(self):
        # Test case where sell_in is exactly 0 (should decrease quality by 4)
        item = Item(name="Conjured", sell_in=0, quality=5)
        conjured_item = ConjuredItem(item)
        quality, sell_in = conjured_item.update_quality()
        self.assertEqual(quality, 1)  # Quality decreases by 4, can't go below 0
        self.assertEqual(sell_in, -1)  # Sell_in decreases by 1

    def test_update_quality_when_quality_is_0(self):
        # Test case where quality is already 0
        item = Item(name="Conjured", sell_in=5, quality=0)
        conjured_item = ConjuredItem(item)
        quality, sell_in = conjured_item.update_quality()
        self.assertEqual(quality, 0)  # Quality remains 0 as it can't go below 0
        self.assertEqual(sell_in, 4)  # Sell_in decreases by 1


if __name__ == "__main__":
    unittest.main()
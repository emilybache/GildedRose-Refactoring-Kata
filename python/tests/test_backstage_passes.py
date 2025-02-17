import unittest
from python.components.gilded_rose import Item
from python.src.backstage_passes import BackstagePassesLogic


class TestBackstagePassesLogic(unittest.TestCase):

    def test_update_quality_before_10_days(self):
        # Test case where sell_in > 10
        item = Item(name="Backstage passes", sell_in=15, quality=10)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 11)  # Quality increases by 1
        self.assertEqual(sell_in, 14)  # Sell_in decreases by 1

    def test_update_quality_before_5_days(self):
        # Test case where sell_in < 10 but > 5
        item = Item(name="Backstage passes", sell_in=9, quality=10)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 12)  # Quality increases by 2 (1 before 10 and 1 before 5)
        self.assertEqual(sell_in, 8)  # Sell_in decreases by 1

    def test_update_quality_on_last_day(self):
        # Test case where sell_in <= 5
        item = Item(name="Backstage passes", sell_in=4, quality=10)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 13)  # Quality increases by 3 (1 before 10, 1 before 5, 1 before 0)
        self.assertEqual(sell_in, 3)  # Sell_in decreases by 1

    def test_update_quality_after_sell_in(self):
        # Test case where sell_in < 0 (should set quality to 0)
        item = Item(name="Backstage passes", sell_in=0, quality=10)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 0)  # Quality should be set to 0 after sell_in < 0
        self.assertEqual(sell_in, -1)  # Sell_in decreases by 1

    def test_update_quality_max_quality(self):
        # Test case where quality is already at 50
        item = Item(name="Backstage passes", sell_in=15, quality=50)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 50)  # Quality should not increase above 50
        self.assertEqual(sell_in, 14)  # Sell_in decreases by 1

    def test_update_quality_when_quality_reaches_max(self):
        # Test case where quality is at 49 and sell_in is under 10 but above 5
        item = Item(name="Backstage passes", sell_in=6, quality=49)
        backstage_pass = BackstagePassesLogic(item)
        quality, sell_in = backstage_pass.update_quality()
        self.assertEqual(quality, 50)  # Quality should max out at 50
        self.assertEqual(sell_in, 5)  # Sell_in decreases by 1


if __name__ == "__main__":
    unittest.main()
import unittest
from python.components.gilded_rose import Item
from python.src.sulfuras import SulfurasLogic


class TestSulfurasLogic(unittest.TestCase):

    def setUp(self):
        # Setup the initial conditions for the tests.
        self.item = Item(name="Sulfuras", sell_in=10, quality=80)
        self.sulfuras = SulfurasLogic(self.item)

    def test_update_quality_does_not_change_quality(self):
        # Ensure that calling update_quality does not change the quality of the item.
        initial_quality = self.item.quality
        self.sulfuras.update_quality()
        self.assertEqual(self.item.quality, initial_quality, "Quality should remain the same.")

    def test_update_quality_does_not_change_sell_in(self):
        # Ensure that calling update_quality does not change the sell_in of the item.
        initial_sell_in = self.item.sell_in
        self.sulfuras.update_quality()
        self.assertEqual(self.item.sell_in, initial_sell_in, "Sell_in should remain the same.")

    def test_initial_values(self):
        # Ensure the initial values of the item are set correctly.
        self.assertEqual(self.item.quality, 80, "Initial quality should be 80.")
        self.assertEqual(self.item.sell_in, 10, "Initial sell_in should be 10.")

if __name__ == '__main__':
    unittest.main()

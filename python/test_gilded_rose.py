import unittest
from gilded_rose import (
    GildedRose,
    Item,
    RegularItem,
    AgedBrie,
    ElixirOfTheMongoose,
    Sulfuras,
    BackstagePass,
    ConjuredItem,
)


class TestGildedRose(unittest.TestCase):
    def setUp(self):
        self.items = [
            Item(name="+5 Dexterity Vest", sell_in=10, quality=20),
            Item(name="Aged Brie", sell_in=2, quality=0),
            Item(name="Elixir of the Mongoose", sell_in=5, quality=7),
            Item(name="Sulfuras, Hand of Ragnaros", sell_in=29, quality=80),
            Item(
                name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20
            ),
            Item(name="Conjured", sell_in=3, quality=13),
        ]

        self.gilded_rose = GildedRose(self.items)

    def test_update_quality(self):
        # Test quality update after 4 days
        for i in range(4):
            self.gilded_rose.update_quality()

        items = self.gilded_rose.get_items()
        self.assertEqual(items[0].quality, 16)
        self.assertEqual(items[0].sell_in, 6)
        self.assertEqual(items[1].quality, 6)
        self.assertEqual(items[1].sell_in, -2)
        self.assertEqual(items[2].quality, 3)
        self.assertEqual(items[2].sell_in, 1)
        self.assertEqual(items[3].quality, 80)
        self.assertEqual(items[3].sell_in, 29)
        self.assertEqual(items[4].quality, 24)
        self.assertEqual(items[4].sell_in, 11)
        self.assertEqual(items[5].quality, 3)
        self.assertEqual(items[5].sell_in, -1)

    def test_special_item(self):
        # Test special item creation
        items = self.gilded_rose._special_items(self.items)
        self.assertIsInstance(items[0], RegularItem)
        self.assertIsInstance(items[1], AgedBrie)
        self.assertIsInstance(items[2], ElixirOfTheMongoose)
        self.assertIsInstance(items[3], Sulfuras)
        self.assertIsInstance(items[4], BackstagePass)
        self.assertIsInstance(items[5], ConjuredItem)

    def test_regular_item(self):
        # Test case 1: check that quality of a RegularItem decreases by 1 per day before sell_in date, and by 2 after
        items = [
            Item(name="+5 Dexterity Vest", sell_in=5, quality=20),
            Item(name="+5 Strength Vest", sell_in=-1, quality=20),
        ]
        gilded_rose = GildedRose(items)
        for days in range(5):
            gilded_rose.update_quality()

        self.assertEqual(gilded_rose.get_items()[0].quality, 15)
        self.assertEqual(gilded_rose.get_items()[1].quality, 10)

    def test_aged_brie(self):
        # Test case 2: check that quality of AgedBrie increases by 1 per day before sell_in date, and by 2 after
        items = [
            Item(name="Aged Brie", sell_in=10, quality=0),
            Item(name="Aged Brie President", sell_in=-1, quality=0),
        ]

        gilded_rose = GildedRose(items)
        for days in range(5):
            gilded_rose.update_quality()

        self.assertEqual(gilded_rose.get_items()[0].quality, 5)
        self.assertEqual(gilded_rose.get_items()[1].quality, 10)

    def test_sulfuras(self):
        # Test case 3: check that Sulfuras never decreases in quality or sell_in
        items = [Item(name="Sulfuras, Hand of Ragnaros", sell_in=10, quality=80)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(gilded_rose.get_items()[0].quality, 80)
        self.assertEqual(gilded_rose.get_items()[0].sell_in, 10)

    def test_backstage_pass(self):
        # Test case 4: check that BackstagePass increases by 1 per day before sell_in date, by 2 before 10 days, and by 3 before 5 days, and quality drops to 0 after the concert
        items = [
            Item(
                name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=10
            )
        ]

        gilded_rose = GildedRose(items)
        for days in range(15, 10, -1):
            gilded_rose.update_quality()
        self.assertEqual(gilded_rose.get_items()[0].quality, 15)

        for days in range(10, 5, -1):
            gilded_rose.update_quality()
        self.assertEqual(gilded_rose.get_items()[0].quality, 25)

        for days in range(5, 0, -1):
            gilded_rose.update_quality()
        self.assertEqual(gilded_rose.get_items()[0].quality, 40)

        for days in range(0, -2, -1):
            gilded_rose.update_quality()
        self.assertEqual(gilded_rose.get_items()[0].quality, 0)

    def test_conjured(self):
        # Test case 5: check that ConjuredItem decreases in quality twice as fast as RegularItem
        items = [
            Item(name="Conjured", sell_in=10, quality=20),
            Item(name="+10 Agility Dagger", sell_in=10, quality=20),
        ]
        gilded_rose = GildedRose(items)
        items = gilded_rose.get_items()
        for days in range(5):
            gilded_rose.update_quality()

        self.assertIsInstance(items[0], ConjuredItem)
        self.assertIsInstance(items[1], RegularItem)
        self.assertEqual(items[0].quality, 10)
        self.assertEqual(items[1].quality, 15)
        self.assertGreater(items[1].quality, items[0].quality)


if __name__ == "__main__":
    unittest.main()

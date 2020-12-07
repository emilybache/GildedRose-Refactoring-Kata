using System;
using NUnit.Framework;
using System.Collections.Generic;

namespace csharpcore
{
    [TestFixture]
    public class GildedRoseTests
    {
        Item _test_ordinary_item_01 = new Item {Name = "+5 Dexterity Vest", SellIn = 10, Quality = 20};
        Item _test_aged_brie = new Item {Name = ItemNames.AgedBrie, SellIn = 2, Quality = 0};
        Item _test_ordinary_item_02 = new Item {Name = "Elixir of the Mongoose", SellIn = 5, Quality = 7};
        Item _test_sulfuras_01 = new Item {Name = ItemNames.Sulfuras, SellIn = 0, Quality = 80};

        [Test]
        public void After_daily_update_item_name_is_not_altered()
        {
            // Arrange
            const String ItemName = "I am a hole, and I live in a mole.";
            IList<Item> items = new List<Item> { new Item { Name = ItemName, SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(ItemName, items[0].Name);
        }

        [TestCase("+5 Dexterity Vest")]
        [TestCase(ItemNames.AgedBrie)]
        [TestCase("Elixir of the Mongoose")]
        [TestCase("some miscellaneous item")]
        [TestCase("Backstage passes to a TAFKAL80ETC concert")]
        [TestCase("Backstage passes to a Bob Marley concert")]
        [TestCase("Backstage passes to a Jungle Boys concert")]
        public void After_daily_update_SellIn_value_for_all_items_except_sulfuras_goes_down_by_one(string item_name)
        {
            // Arrange
            int initial_sellin_value = 10;
            IList<Item> items = new List<Item> { new Item { Name = item_name, SellIn = initial_sellin_value, Quality = 20 } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_sellin_value - 1, items[0].SellIn);
        }

        [Test]
        public void After_daily_update_SellIn_value_for_sulfuras_remains_the_same()
        {
            // Arrange
            int initial_sellin_value = _test_sulfuras_01.SellIn;
            IList<Item> items = new List<Item> { _test_sulfuras_01 };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_sellin_value, items[0].SellIn);
        }

        [Test]
        public void After_daily_update_Quality_value_for_sulfuras_remains_the_same()
        {
            // Arrange
            int initial_quality_value = _test_sulfuras_01.Quality;
            IList<Item> items = new List<Item> { _test_sulfuras_01 };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_quality_value, items[0].Quality);
        }

        [Test]
        public void After_daily_update_Quality_value_for_ordinary_items_goes_down_by_one()
        {
            // Arrange
            int initial_quality_value_01 = _test_ordinary_item_01.Quality;
            int initial_quality_value_02 = _test_ordinary_item_02.Quality;
            IList<Item> items = new List<Item> { _test_ordinary_item_01, _test_ordinary_item_02 };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_quality_value_01 - 1, items[0].Quality);
            Assert.AreEqual(initial_quality_value_02 - 1, items[1].Quality);
        }

        [Test]
        public void After_daily_update_Quality_value_for_aged_brie_goes_up_by_one()
        {
            // Arrange
            int initial_quality_value = _test_aged_brie.Quality;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.AgedBrie, SellIn = 20, Quality = initial_quality_value } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_quality_value + 1, items[0].Quality);
        }

        [Test]
        public void Once_the_sell_by_date_has_passed_aged_brie_quality_goes_up_by_two_each_day()
        {
            // Arrange
            int initial_quality_value = _test_aged_brie.Quality;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.AgedBrie, SellIn = -2, Quality = initial_quality_value } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(initial_quality_value + 2, items[0].Quality);
        }

        [Test]
        public void Once_the_sell_by_date_has_passed_aged_brie_quality_will_not_go_above_50()
        {
            // Arrange
            int initial_quality_value = 50;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.AgedBrie, SellIn = -2, Quality = initial_quality_value } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.IsTrue(items[0].Quality <= 50);
        }

        [TestCase("+5 Dexterity Vest")]
        [TestCase(ItemNames.AgedBrie)]
        [TestCase(ItemNames.Sulfuras)]
        [TestCase("Elixir of the Mongoose")]
        [TestCase("some miscellaneous item")]
        [TestCase("Backstage passes to a TAFKAL80ETC concert")]
        [TestCase("Backstage passes to a Bob Marley concert")]
        [TestCase("Backstage passes to a Jungle Boys concert")]
        public void The_quality_of_all_items_except_sulfuras_never_goes_above_50(string item_name)
        {
            // Arrange
            int initial_quality_value = 50;
            IList<Item> items = new List<Item> { new Item { Name = item_name, SellIn = 10, Quality = initial_quality_value } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.IsTrue(items[0].Quality <= 50);
        }

        [TestCase("+5 Dexterity Vest", 10)]
        [TestCase(ItemNames.AgedBrie, 10)]
        [TestCase(ItemNames.Sulfuras, 10)]
        [TestCase("Elixir of the Mongoose", 10)]
        [TestCase("some miscellaneous item", 10)]
        [TestCase("some miscellaneous item", -2)]
        [TestCase("Backstage passes to a TAFKAL80ETC concert", 10)]
        [TestCase("Backstage passes to a Bob Marley concert", 10)]
        [TestCase("Backstage passes to a Jungle Boys concert", 10)]
        public void The_quality_of_an_item_is_never_negative(string item_name, int sellin)
        {
            // Arrange
            int initial_quality_value = 0;
            IList<Item> items = new List<Item> { new Item { Name = item_name, SellIn = sellin, Quality = initial_quality_value } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.IsTrue(items[0].Quality >= 0);
        }

        [TestCase("+5 Dexterity Vest")]
        [TestCase("Elixir of the Mongoose")]
        [TestCase("some miscellaneous item")]
        public void Once_a_sell_by_date_is_passed_the_quality_of_ordinary_items_decreases_by_two_per_day(string item_name)
        {
            // Arrange
            const int InitialQualityValue = 10;
            const int PastSellByDate = 0;
            IList<Item> items = new List<Item> { new Item { Name = item_name, SellIn = PastSellByDate, Quality = InitialQualityValue } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(InitialQualityValue - 2, items[0].Quality);
        }

        [TestCase(50)]
        [TestCase(30)]
        [TestCase(11)]
        public void After_daily_update_Quality_value_for_backstage_pass_goes_up_by_one_if_more_than_ten_days_to_go(int sellin)
        {
            // Arrange
            const int InitialQualityValue = 10;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.BackstagePasses, SellIn = sellin, Quality = InitialQualityValue } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(InitialQualityValue + 1, items[0].Quality);
        }

        [TestCase(10)]
        [TestCase(9)]
        [TestCase(8)]
        [TestCase(7)]
        [TestCase(6)]
        public void quality_of_backstage_pass_increases_by_two_each_day_between_ten_days_and_six_days_before_concert_date(int sellin)
        {
            // Arrange
            const int InitialQualityValue = 10;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.BackstagePasses, SellIn = sellin, Quality = InitialQualityValue } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(InitialQualityValue + 2, items[0].Quality);
        }

        [TestCase(5)]
        [TestCase(4)]
        [TestCase(3)]
        [TestCase(2)]
        [TestCase(1)]
        public void quality_of_backstage_pass_increases_by_three_each_day_five_days_or_less_before_concert_date(int sellin)
        {
            // Arrange
            const int InitialQualityValue = 10;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.BackstagePasses, SellIn = sellin, Quality = InitialQualityValue } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(InitialQualityValue + 3, items[0].Quality);
        }

        [Test]
        public void quality_of_backstage_pass_drops_to_zero_after_concert_date()
        {
            // Arrange
            const int InitialQualityValue = 10;
            const int PastConcertDate = 0;
            IList<Item> items = new List<Item> { new Item { Name = ItemNames.BackstagePasses, SellIn = PastConcertDate, Quality = InitialQualityValue } };
            GildedRose app = new GildedRose(items);

            // Act
            app.UpdateQuality();

            // Assert
            Assert.AreEqual(0, items[0].Quality);
        }
    }
}

using NUnit.Framework;
using System.Collections.Generic;

namespace csharp
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        public void UpdateQuality_LowersItemQualityBy1()
        {
            var item = new Item
            {
                Name = "test",
                Quality = 5,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(4));
        }

        [Test]
        public void UpdateQuality_LowersItemSellInBy1()
        {
            var item = new Item
            {
                Name = "test",
                Quality = 5,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.SellIn, Is.EqualTo(29));
        }

        [Test]
        public void UpdateQuality_ItemWithZeroSellIn_LowersItemQualityBy2()
        {
            var item = new Item
            {
                Name = "test",
                Quality = 5,
                SellIn = 0
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(3));
        }

        [Test]
        public void UpdateQuality_ItemWithZeroQuality_StayAtZeroQuality()
        {
            var item = new Item
            {
                Name = "test",
                Quality = 0,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(0));
        }

        [Test]
        public void UpdateQuality_AgedBrie_IncreaseQualityBy1()
        {
            var item = new Item
            {
                Name = "Aged Brie",
                Quality = 0,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(1));
        }

        [Test]
        public void UpdateQuality_AgedBrieWith50Quality_StayAt50Quality()
        {
            var item = new Item
            {
                Name = "Aged Brie",
                Quality = 50,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(50));
        }

        [Test]
        public void UpdateQuality_Sulfuras_DoesNotDecreaseQuality()
        {
            var item = new Item
            {
                Name = "Sulfuras, Hand of Ragnaros",
                Quality = 80,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(80));
        }

        [Test]
        public void UpdateQuality_Sulfuras_DoesNotDecreaseSellIn()
        {
            var item = new Item
            {
                Name = "Sulfuras, Hand of Ragnaros",
                Quality = 80,
                SellIn = 30
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.SellIn, Is.EqualTo(30));
        }

        [Test]
        public void UpdateQuality_BackstagePassesWithMoreThan10DaysLeft_IncreasesQualityBy1()
        {
            var item = new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                Quality = 35,
                SellIn = 11
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(36));
        }

        [Test]
        public void UpdateQuality_BackstagePassesWith10DaysLeft_IncreasesQualityBy2()
        {
            var item = new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                Quality = 35,
                SellIn = 10
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(37));
        }

        [Test]
        public void UpdateQuality_BackstagePassesWith5DaysLeft_IncreasesQualityBy3()
        {
            var item = new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                Quality = 35,
                SellIn = 5
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(38));
        }

        [Test]
        public void UpdateQuality_BackstagePassesWith0DaysLeft_SetQualityToZero()
        {
            var item = new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                Quality = 35,
                SellIn = 0
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(0));
        }

        [Test]
        public void UpdateQuality_ConjuredItemWithSellIn4_DecreasesQualityBy2()
        {
            var item = new Item
            {
                Name = "Conjured Mana Cake",
                Quality = 35,
                SellIn = 4
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(33));
        }

        [Test]
        public void UpdateQuality_ConjuredItemWithSellIn0_DecreasesQualityBy4()
        {
            var item = new Item
            {
                Name = "Conjured Mana Cake",
                Quality = 35,
                SellIn = 0
            };

            var items = new List<Item>(new[] { item });
            var gRose = new GildedRose(items);

            gRose.UpdateQuality();

            Assert.That(item.Quality, Is.EqualTo(31));
        }
    }
}

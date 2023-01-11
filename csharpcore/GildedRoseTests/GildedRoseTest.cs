using NUnit;
using System.Collections.Generic;
using System.Linq;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests
{
    [TestFixture]
    public class GildedRoseUpdateQuality
    {
        [SetUp]
        public void Setup()
        {
        }

        private List<Item> ResetItems()
        {
            return new List<Item>
            {
                new Item { Name = "+5 Dexterity Vest", SellIn = 10, Quality = 20 },
                new Item { Name = "Aged Brie", SellIn = 2, Quality = 0 },
                new Item { Name = "Elixir of the Mongoose", SellIn = 5, Quality = 7 },
                new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80 },
                new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = -1, Quality = 80 },
                new Item
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 15,
                    Quality = 20
                },
                new Item
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 10,
                    Quality = 49
                },
                new Item
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 5,
                    Quality = 49
                },
                // this conjured item does not work properly yet
                new Item { Name = "Conjured Mana Cake", SellIn = 3, Quality = 6 }
            };
        }

        [TestCase("Sulfuras, Hand of Ragnaros", 10, 80, 80)]
        [TestCase("Elixir of the Mongoose", 10, 7, 6)]
        [TestCase("Aged Brie", 10, 0, 1)]
        [TestCase("Backstage passes to a TAFKAL80ETC concert",15,10,11)]
        [TestCase("Backstage passes to a TAFKAL80ETC concert",10,10,12)]
        [TestCase("Backstage passes to a TAFKAL80ETC concert",5,10,13)]
        [TestCase("Backstage passes to a TAFKAL80ETC concert",0,10,0)]
        [TestCase("Conjured Mana Cake", 10,10,8)]
        public void ShouldUpdateQuality_BeforeSellByDate(string name, int sellInDays, int startQuality, int endQuality)
        {
            //Arrange
            var item = new Item
            {
                Name = name,
                Quality = startQuality,
                SellIn = sellInDays
            };
            var items = new List<Item>() { item };
            var app = new GildedRose(items);
            //Act
            app.UpdateQuality();
            //Assert
            var updatedItem = items.FirstOrDefault(x => x.Name == name);
            Assert.NotNull(updatedItem);
            Assert.AreEqual(endQuality, updatedItem.Quality);
        }

        [TestCase("Sulfuras, Hand of Ragnaros", 0, 80, 80)]
        [TestCase("Elixir of the Mongoose", 0, 7, 5)]
        [TestCase("Aged Brie", 0, 0, 2)]
        [TestCase("Conjured Mana Cake", 0, 10, 6)]
        public void ShouldUpdateQualityDifferently_AfterSellByDate(string name, int sellInDays, int startQuality, int endQuality)
        {
            //Arrange
            var item = new Item
            {
                Name = name,
                Quality = startQuality,
                SellIn = sellInDays
            };
            var items = new List<Item>() { item };
            var app = new GildedRose(items);
            //Act
            app.UpdateQuality();
            //Assert
            var updatedItem = items.FirstOrDefault();
            Assert.NotNull(updatedItem);
            Assert.AreEqual(endQuality, updatedItem.Quality);
        }

        [TestCase("Sulfuras, Hand of Ragnaros", 80)]
        [TestCase("Elixir of the Mongoose", 0)]
        [TestCase("Aged Brie", 0)]
        [TestCase("Conjured Mana Cake", 0)]
        public void ShouldNotAllowNegativeQuality(string name, int startQuality)
        {
            //Arrange
            var item = new Item
            {
                Name = name,
                Quality = startQuality,
                SellIn = 10
            };
            var items = new List<Item>() { item };
            var app = new GildedRose(items);
            //Act
            app.UpdateQuality();
            
            //Assert
            var updatedItem = items.FirstOrDefault();
            Assert.GreaterOrEqual(item.Quality, 0);
        }
        [TestCase("Sulfuras, Hand of Ragnaros", 80)]
        [TestCase("Elixir of the Mongoose")]
        [TestCase("Aged Brie")]
        [TestCase("Conjured Mana Cake")]

        public void ShouldNotExceedMaximumQuality(string name, int maxQuality=50)
        {
            //Arrange
            var item = new Item
            {
                Name = name,
                Quality = maxQuality,
                SellIn = 10
            };
            var items = new List<Item>() { item };
            var app = new GildedRose(items);
            //Act
            app.UpdateQuality();

            //Assert
            var updatedItem = items.FirstOrDefault();
            Assert.LessOrEqual(item.Quality, maxQuality);
        }



    }
}
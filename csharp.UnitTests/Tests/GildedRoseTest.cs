/*
 * REQUIREMENTS
 *
 *Pretty simple, right? Well this is where it gets interesting:
   
   V - Once the sell by date has passed, Quality degrades twice as fast
   V - The Quality of an item is never negative
   V - "Aged Brie" actually increases in Quality the older it gets
   V - The Quality of an item is never more than 50
   V - "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
   V - "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
   Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
   Quality drops to 0 after the concert
   
   We have recently signed a supplier of conjured items. This requires an update to our system:
   
   V - "Conjured" items degrade in Quality twice as fast as normal items
 *
 */

using System.Collections.Generic;
using NUnit.Framework;

namespace csharp.UnitTests.Tests
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        // [MethodName]_[Scenario]_[ExpectedResult]
        public void UpdateQuality_CreateOneItem_NameIsNotChanged()
        {
            // Arrange
            IList<Item> items = new List<Item> { new Item { Name = "Bread", SellIn = 0, Quality = 0 } };
            var gildedRose = new GildedRose(items);

            // Act
            gildedRose.UpdateQuality();

            // Assert
            Assert.AreEqual("Bread", items[0].Name);
        }

        [Test]
        public void UpdateQuality_CreateItemWithQualityZero_QualityIsNeverNegative()
        {
            IList<Item> items = new List<Item> { new Item { Name = "Bread", SellIn = 0, Quality = 0 } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(0));
        }

        [Test]
        public void UpdateQuality_SellByDateHasPassed_QualityDegradesTwiceAsFast()
        {
            IList<Item> items = new List<Item> { new Item { Name = "Bread", SellIn = 0, Quality = 10 } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(8));
        }

        [Test]
        [TestCase(0, 0, 2)] 
        [TestCase(1, 1, 2)]                                                                                    
        public void UpdateQuality_AgedBrieGetsOlder_AgedBrieIncreasesInQuality(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Aged Brie", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();
            
            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(5, 50, 50)]
        [TestCase(-5, 50, 50)]
        [TestCase(-5, 49, 50)]
        public void UpdateQuality_AgedBrieGetsOlder_AgedBrieIncreasesInQualityMaxUpTo50(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Aged Brie", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(5, 5, 5)]
        [TestCase(-5, -5, -5)]
        [TestCase(0, 0, 0)]
        public void UpdateQuality_SulfurasIsNeverSold_SellInStaysTheSame(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].SellIn, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(5, 5, 5)]
        [TestCase(-5, -5, -5)]
        [TestCase(0, 0, 0)]
        public void UpdateQuality_SulfurasNeverDecreasesInQuality_QualityInStaysTheSame(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(20, 50, 50)]
        [TestCase(10, 50, 50)]
        [TestCase(5, 50, 50)]
        [TestCase(-10, 50, 0)]
        [TestCase(20, 40, 41)]
        public void UpdateQuality_BackstagePassesGetsOlder_BackstagePassesIncreasesInQualityMaxUpTo50(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(10, 40, 42)]
        public void UpdateQuality_BackstagePassesLessThen10SellIn_QualityIncreasesBy2(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(5, 40, 43)]
        public void UpdateQuality_BackstagePassesLessThen5SellIn_QualityIncreasesBy2(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }


        [TestCase(0, 25, 0)]
        [TestCase(0, 50, 0)]
        public void UpdateQuality_BackstagePassesConcertEnds_QualityDropsToZero(int sellIn, int quality, int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }

        [Test]
        [TestCase(-1, 20, 18)]
        public void UpdateQuality_ConjuredGetsOlder_ConjuredQualityDegradesTwoTimesFaster(int sellIn, int quality,
            int expectedResult)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Conjured Mana Cake", SellIn = sellIn, Quality = quality } };
            var gildedRose = new GildedRose(items);

            gildedRose.UpdateQuality();

            Assert.That(items[0].Quality, Is.EqualTo(expectedResult));
        }
    }
}

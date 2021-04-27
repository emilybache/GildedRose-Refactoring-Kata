using Microsoft.VisualStudio.TestTools.UnitTesting;
using csharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csharp.Tests
{
    [TestClass()]
    public class GildedRoseTests
    {

        [TestMethod()]
        public void DefaultTest() // Test that came with app
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual("Aged Brie", Items[0].Name);
        }

        [TestMethod()]
        public void UpdateQualityTestForAgedBrie_SellDateIsLessThan10ButGreaterThan5_IncreaseQualityByTwo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 10, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(2, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTestForAgedBrie_SellDateIsLessThan5_IncreaseQualityByThree()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 5, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(3, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTestForAgedBrie_AfterConcert_DropToZero()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 0, Quality = 10 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_SellDatePassed_DecreaseQualityTwice()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 0, Quality = 2 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_QualityOfItemIsNeverNegative_MinimumValue0()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "+5 Dexterity Vest", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_QualityOfItemNeverAboveDefinedValue_MaximumValue50()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 1, Quality = 49 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(50, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_LegendaryItems_NeverAltar()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(80, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_LegendaryItems_MaximumValue80()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 1, Quality = 80 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(80, Items[0].Quality);
        }

        [TestMethod()]
        public void UpdateQualityTest_ReduceSellIn_By1Daily()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 1, Quality = 30 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].SellIn);
        }
    }
}
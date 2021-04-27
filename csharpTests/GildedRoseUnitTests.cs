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
        public void UpdateQualityTestForAgedBrie_SellDatePassed_IncreaseQualityByTwo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(2, Items[0].Quality);
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
        public void UpdateQualityTest_QualityOfItemIsNeverNegative_MinimumValue()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "+5 Dexterity Vest", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].Quality);
        }

        public void UpdateQualityTest_QualityOfItemNeverAboveDefinedValue_MaximumValue()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "+5 Dexterity Vest", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);

            app.UpdateQuality();

            Assert.AreEqual(0, Items[0].Quality);
        }
    }
}
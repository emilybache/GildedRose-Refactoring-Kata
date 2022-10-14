using NUnit.Framework;
using System.Collections.Generic;

namespace csharp
{
    [TestFixture]
    public class GildedRoseTest
    {
        private IList<Item> Items = new List<Item> { new Item { Name = "", SellIn = 0, Quality = 0 } };


        [Test]
        public void foo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.AreEqual("fixme", Items[0].Name);
        }
        /*
        [Test]
        public void ConjuredFood_SellInGreaterThanOrSameAs1_QualityLoweredBy2()
        {
            //Arrange
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured Meat", SellIn = 1, Quality = 3 },
                                                 new Item { Name = "conjured Fish", SellIn = 2, Quality = 3 },
                                                 new Item { Name = "Conjured Water", SellIn = 1, Quality = 0 },
                                                 new Item { Name = "Pie Conjured", SellIn = 1, Quality = 1 }};
            GildedRose app = new GildedRose(Items);

            //Act
            app.UpdateQuality();

            //Assert
            Assert.AreEqual(1, Items[0].Quality);
            Assert.AreEqual(1, Items[1].Quality);
            Assert.AreEqual(0, Items[2].Quality);
            Assert.AreEqual(0, Items[3].Quality);
        }

        [Test]
        public void ConjuredFood_SellInLessThanOrSameAs0_QualityLowerBy4()
        {
            //Arrange
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured Meat", SellIn = -1, Quality = 3 },
                                                 new Item { Name = "conjured Fish", SellIn = 0, Quality = 5 } };
            GildedRose app = new GildedRose(Items);

            //Act
            app.UpdateQuality();

            //Assert
            Assert.AreEqual(0, Items[0].Quality);
            Assert.AreEqual(1, Items[1].Quality);
        }
        */
        [TestCase("Conjured Meat", 1, 3, ExpectedResult = 1)]
        [TestCase("conjured Fish", 2, 3, ExpectedResult = 1)]
        [TestCase("Conjured Water", 1, 0, ExpectedResult = 0)]
        [TestCase("Pie Conjured", 1, 1, ExpectedResult = 0)]
        [TestCase("Conjured Meat", 0, 3, ExpectedResult = 0)]
        [TestCase("Conjured Water", -1, 5, ExpectedResult = 1)]
        public int ConjuredFood_LoweringQuality(string name,int sellIn, int quality)
        {
            //arrange
            GildedRose app = new GildedRose(Items);
            Items[0].Name = name;
            Items[0].SellIn = sellIn;
            Items[0].Quality = quality;
            //act
            app.UpdateQuality();
            //assert
            return Items[0].Quality;
        }
    }
}

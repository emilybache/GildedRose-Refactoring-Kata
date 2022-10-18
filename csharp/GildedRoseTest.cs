using NUnit.Framework;
using System.Collections.Generic;
using Moq;

namespace csharp
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        public void foo()
        {
            List<IItem> Items = new List<IItem> { new Item("foo",0,0)};
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            //Assert.AreEqual("fixme", Items[0].Name);
        }
        /*
        [Test]
        public void ConjuredFood_SellInGreaterThanOrSameAs1_QualityLoweredBy2()
        {
            Arrange
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured Meat", SellIn = 1, Quality = 3 },
                                                 new Item { Name = "conjured Fish", SellIn = 2, Quality = 3 },
                                                 new Item { Name = "Conjured Water", SellIn = 1, Quality = 0 },
                                                 new Item { Name = "Pie Conjured", SellIn = 1, Quality = 1 }};
            GildedRose app = new GildedRose(Items);

            Act
            app.UpdateQuality();

            Assert
            Assert.AreEqual(1, Items[0].Quality);
            Assert.AreEqual(1, Items[1].Quality);
            Assert.AreEqual(0, Items[2].Quality);
            Assert.AreEqual(0, Items[3].Quality);
        }

        [Test]
        public void ConjuredFood_SellInLessThanOrSameAs0_QualityLowerBy4()
        {
            Arrange
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured Meat", SellIn = -1, Quality = 3 },
                                                 new Item { Name = "conjured Fish", SellIn = 0, Quality = 5 } };
            GildedRose app = new GildedRose(Items);

            Act
            app.UpdateQuality();

            Assert
            Assert.AreEqual(0, Items[0].Quality);
            Assert.AreEqual(1, Items[1].Quality);
        }
        */
        [TestCase("meat", 1, 3, ExpectedResult = 1)]
        [TestCase("meat", 1, 1, ExpectedResult = 0)]
        [TestCase("meat", 0, 3, ExpectedResult = 0)]
        [TestCase("meat", -1, 5, ExpectedResult = 1)]
        public int ConjuredFood_LoweringQuality(string name, int sellIn, int quality)
        {
            //arrange
            List<IItem> Items = new List<IItem> { new Conjured(name, sellIn, quality) };
            GildedRose app = new GildedRose(Items);
            //act
            app.UpdateQuality();
            //assert
            return Items[0].GetQuality();
        }
    }
}

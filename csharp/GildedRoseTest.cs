using NUnit.Framework;
using System.Collections.Generic;
using Moq;

namespace csharp
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        //to update
        public void foo()
        {
            List<IItem> Items = new List<IItem> { new Item("foo",0,0)};
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            //Assert.AreEqual("fixme", Items[0].Name);
        }

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

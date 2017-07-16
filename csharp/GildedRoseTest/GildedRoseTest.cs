using GildedRose;
using NUnit.Framework;
using System.Collections.Generic;

namespace GildedRoseTest
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        public void foo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
            GildedRose.GildedRose app = new GildedRose.GildedRose(Items);
            app.UpdateQuality();
            Assert.AreEqual("fixme", Items[0].Name);
        }
    }
}

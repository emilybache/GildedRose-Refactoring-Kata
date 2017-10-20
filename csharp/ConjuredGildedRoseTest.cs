using NUnit.Framework;
using System.Collections.Generic;

namespace csharp
{
    [TestFixture]
    public class ConjuredGildedRoseTest
    {
        [Test]
        public void Quality()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured Mana Cake", SellIn = 1, Quality = 8 } };
            GildedRose app = new GildedRose(Items);

            // "Conjured" items degrade in Quality twice as fast as normal items. 
            // So, 
            //      SellIn >= 0 => degrade = -2 
            //      SellIn < 0 => degrade = -4
            app.UpdateQuality();
            Assert.AreEqual(6, Items[0].Quality);

            app.UpdateQuality();
            Assert.AreEqual(2, Items[0].Quality);
        }
    }
}

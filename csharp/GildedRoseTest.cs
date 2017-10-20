using csharp.StrategyPatternExample;
using NUnit.Framework;
using System.Collections.Generic;

namespace csharp
{
    [TestFixture]
    public class GildedRoseTest
    {
        [Test]
        public void ConjuredQuality()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Global.NAME_ITEM_CONJURED, SellIn = 1, Quality = 8 } };
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

        [Test]
        public void ConjuredQuality_StrategyPatternExample()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Global.NAME_ITEM_CONJURED, SellIn = 1, Quality = 8 } };
            GildedRoseStrategyPatternExample app = new GildedRoseStrategyPatternExample(Items);

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

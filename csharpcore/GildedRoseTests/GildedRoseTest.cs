using Xunit;
using System.Collections.Generic;
using GildedRoseKata;

namespace GildedRoseTests
{
    public class GildedRoseTest
    {
        [Fact]
        public void foo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
            GildedRoseKata.GildedRose app = new GildedRoseKata.GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal("fixme", Items[0].Name);
        }
    }
}

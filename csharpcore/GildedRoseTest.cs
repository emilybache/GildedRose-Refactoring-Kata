using Xunit;
using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRoseTest
    {
        [Fact]
        public void foo()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };

            GildedRose.Items = Items;
            GildedRose.UpdateQuality();

            Assert.Equal("foo", Items[0].Name);
        }
    }
}
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
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal("foo", Items[0].Name);
        }
        //Once the sell by date has passed, Quality degrades twice as fast
        [Fact]
        public void UpdateQuality_Should_Return_ExpiredItem_Quality4_Become_Quality2()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "BigStuff", SellIn = -1, Quality = 4 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2,Items[0].Quality);
        }
        //The Quality of an item is never negative
        [Fact]
        public void UpdateQuality_Should_Return_Item_Quality_Never_Negative()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "BigStuff", SellIn = 1, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.False(Items[0].Quality < 0);
        }
        //“Aged Brie” actually increases in Quality the older it gets
        [Fact]
        public void UpdateQuality_Should_Return_AgedBrie_Quality1()
        {
            IList<Item> Items = new List<Item> { new AgingItem { Name = "Aged Brie", SellIn = 2, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(1, Items[0].Quality);
        }
        //“Sulfuras”, being a legendary item, never has to be sold or decreases in Quality
        [Fact]
        public void UpdateQuality_Should_Return_LegendaryItem_Unchanged()
        {
            IList<Item> Items = new List<Item> { new LegendaryItem { Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(80, Items[0].Quality);
            Assert.Equal(0, Items[0].SellIn);
        }
        //“Backstage passes” Quality increases by 2 when there are 10 days or less
        [Fact]
        public void UpdateQuality_Should_Return_Passes_IncreasesBy2_When_SellinLessThen10()
        {
            IList<Item> Items = new List<Item> { new ConcertPass { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 9, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(22, Items[0].Quality);
        }
        //“Backstage passes” Quality increases by 3 when there are 5 days or less
        [Fact]
        public void UpdateQuality_Should_Return_Passes_IncreasesBy3_When_SellinLessThen5()
        {
            IList<Item> Items = new List<Item> { new ConcertPass { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 4, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(23, Items[0].Quality);
        }
        //“Backstage passes” Quality drops to 0 after the concert
        [Fact]
        public void UpdateQuality_Should_Return_PassesQuality0_When_Sellin_LessThan0()
        {
            IList<Item> Items = new List<Item> { new ConcertPass { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = -1, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(0, Items[0].Quality);
        }
        //“Conjured” items degrade in Quality twice as fast as normal items
        [Fact]
        public void UpdateQuality_Should_Return_ConjuredQuality0_When_Quality2()
        {
            IList<Item> Items = new List<Item> { new ConjuredItem { Name = "Conjured Mana Cake", SellIn = 3, Quality = 2 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(0, Items[0].Quality);
        }


    }
}

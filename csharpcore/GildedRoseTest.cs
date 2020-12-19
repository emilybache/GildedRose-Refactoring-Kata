using Xunit;
using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRoseTest
    {
        [Fact]
        public void UpdateQuality_NoItems_YieldNoException()
        {
            IList<Item> items = new List<Item> { };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();
        }

        [Fact]
        public void UpdateQuality_SulfuraItem_Unchanged()
        {
            IList<Item> items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 10, Quality = 80 } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(80, items[0].Quality);
            Assert.Equal(10, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 7, 8)]
        [InlineData(0, 7, 8)] // Issue with Aged Brie that increase Quality twice as fast after SellIn, correct ?
        public void UpdateQuality_AgedBrie_QualityIncreases(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Aged Brie", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 50, 50)]
        [InlineData(0, 50, 50)]
        public void UpdateQuality_AgedBrieMaxQuality_QualityToppedAtFifty(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Aged Brie", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(11, 7, 8)]
        [InlineData(10, 7, 9)]
        [InlineData(5, 7, 10)]
        public void UpdateQuality_BackStagePass_QualityIncreases(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(0, 7, 0)]
        [InlineData(0, 50, 0)]
        public void UpdateQuality_BackStagePassAfterConcert_QualityZeroed(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(11, 50, 50)]
        [InlineData(10, 50, 50)]
        [InlineData(5, 50, 50)]
        public void UpdateQuality_BackStagePassMaxQuality_QualityToppedAtFifty(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 10, 8)]
        [InlineData(0, 10, 6)]
        public void UpdateQuality_ConjuredItem_QualityDecreases(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Old Conjured Helmet", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 0, 0)]
        [InlineData(0, 0, 0)]
        public void UpdateQuality_ConjuredItemMinQuality_QualityCannotGoNegative(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Conjured Cake the is a lie !", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 10, 9)]
        [InlineData(0, 10, 8)]
        public void UpdateQuality_MiscelaniousItem_QualityDecreases(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Wooden Sword", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }

        [Theory]
        [InlineData(7, 0, 0)]
        [InlineData(0, 0, 0)]
        public void UpdateQuality_MiscelaniousItemMinQuality_QualityCannotGoNegative(int beforeSellIn, int beforeQuality, int afterQuality)
        {
            IList<Item> items = new List<Item> { new Item { Name = "Leeroy Jenkins Trinket", SellIn = beforeSellIn, Quality = beforeQuality } };
            GildedRose app = new GildedRose(items);

            app.UpdateQuality();

            Assert.Equal(afterQuality, items[0].Quality);
            Assert.Equal(beforeSellIn - 1, items[0].SellIn);
        }
    }
}
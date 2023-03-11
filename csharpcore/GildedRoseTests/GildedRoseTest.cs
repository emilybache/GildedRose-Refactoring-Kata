using Xunit;
using System.Collections.Generic;
using GildedRoseKata;

namespace GildedRoseTests
{
    public class GildedRoseTest
    {
        [Fact]
        public void standard_product_quality_decrease_by_one_before_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 3, Quality = 5 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(4, Items[0].Quality);
        }

        [Fact]
        public void standard_product_quality_decrease_by_two_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 5 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(3, Items[0].Quality);
        }

        [Fact]
        public void standard_product_with_0_quality_is_still_0_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(0, Items[0].Quality);
        }

        [Fact]
        public void product_quality_is_never_negative()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 3, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(0, Items[0].Quality);
        }

        [Fact]
        public void aged_brie_increase_in_quality_before_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 3, Quality = 8 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(9, Items[0].Quality);
        }

        [Fact]
        public void aged_brie_increase_in_quality_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 0, Quality = 10 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(12, Items[0].Quality);
        }

        [Fact]
        public void aged_brie_quality_do_not_exceed_50()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 3, Quality = 50 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(50, Items[0].Quality);
        }

        [Fact]
        public void product_with_initial_quality_55_decrease()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 3, Quality = 55 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(54, Items[0].Quality);
        }

        [Fact]
        public void sulfuras_has_constant_quality_before_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 3, Quality = 80 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(3, Items[0].SellIn);
            Assert.Equal(80, Items[0].Quality);
        }

        [Fact]
        public void sulfuras_has_constant_quality_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(0, Items[0].SellIn);
            Assert.Equal(80, Items[0].Quality);
        }

        [Fact]
        public void backstage_passes_quality_decrease_when_sell_by_date_is_more_then_10()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 15, Quality = 30 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(14, Items[0].SellIn);
            Assert.Equal(31, Items[0].Quality);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_2_when_sell_by_date_is_between_5_and_10()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 10, Quality = 30 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(9, Items[0].SellIn);
            Assert.Equal(32, Items[0].Quality);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_3_when_sell_by_date_is_less_than_5()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 5, Quality = 30 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(4, Items[0].SellIn);
            Assert.Equal(33, Items[0].Quality);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_3_when_sell_by_date_is_1()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 1, Quality = 30 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(0, Items[0].SellIn);
            Assert.Equal(33, Items[0].Quality);
        }

        [Fact]
        public void backstage_passes_quality_is_0_when_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 0, Quality = 30 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(0, Items[0].Quality);
        }

        [Fact]
        public void conjured_quality_decrease_by_two_before_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured", SellIn = 3, Quality = 5 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(2, Items[0].SellIn);
            Assert.Equal(3, Items[0].Quality);
        }

        [Fact]
        public void conjured_quality_decrease_by_four_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured", SellIn = 0, Quality = 5 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(1, Items[0].Quality);
        }

        [Fact]
        public void conjured_with_0_quality_is_still_0_after_sell_by_date_has_passed()
        {
            IList<Item> Items = new List<Item> { new Item { Name = "Conjured", SellIn = 0, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal(-1, Items[0].SellIn);
            Assert.Equal(0, Items[0].Quality);
        }
    }
}

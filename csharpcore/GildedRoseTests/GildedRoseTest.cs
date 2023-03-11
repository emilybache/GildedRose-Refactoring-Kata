using Xunit;
using System.Collections.Generic;
using GildedRoseKata;

namespace GildedRoseTests
{
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "<Pending>")]
    public class GildedRoseTest
    {
        private const string BACKSTAGE_ITEMNAME = "Backstage passes to a TAFKAL80ETC concert";
        private const string DEFAULT_ITEMNAME = "foo";
        private const string AGED_BRIE_ITEMNAME = "Aged Brie";
        private const string SULFURAS_ITEMNAME = "Sulfuras, Hand of Ragnaros";
        private const string CONJURED_ITEMNAME = "Conjured";

        [Fact]
        public void standard_product_quality_decrease_by_one_before_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(DEFAULT_ITEMNAME, 3, 5);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 4);
        }

        [Fact]
        public void standard_product_quality_decrease_by_two_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(DEFAULT_ITEMNAME, 0, 5);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 3);
        }

        [Fact]
        public void standard_product_with_0_quality_is_still_0_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(DEFAULT_ITEMNAME, 0, 0);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 0);
        }

        [Fact]
        public void product_quality_is_never_negative()
        {
            IList<Item> Items = CreateItems(DEFAULT_ITEMNAME, 3, 0);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 0);
        }

        [Fact]
        public void aged_brie_increase_in_quality_before_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(AGED_BRIE_ITEMNAME, 3, 8);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 9);
        }

        [Fact]
        public void aged_brie_increase_in_quality_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(AGED_BRIE_ITEMNAME, 0, 10);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 12);
        }

        [Fact]
        public void aged_brie_quality_do_not_exceed_50()
        {
            IList<Item> Items = CreateItems(AGED_BRIE_ITEMNAME, 3, 50);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 50);
        }

        [Fact]
        public void product_with_initial_quality_55_decrease()
        {
            IList<Item> Items = CreateItems(DEFAULT_ITEMNAME, 3, 55);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 54);
        }

        [Fact]
        public void sulfuras_has_constant_quality_before_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(SULFURAS_ITEMNAME, 3, 80);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 3, expectedQuality: 80);
        }

        [Fact]
        public void sulfuras_has_constant_quality_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(SULFURAS_ITEMNAME, 0, 80);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 0, expectedQuality: 80);
        }

        [Fact]
        public void backstage_passes_quality_decrease_when_sell_by_date_is_more_then_10()
        {
            IList<Item> Items = CreateItems(BACKSTAGE_ITEMNAME, 15, 30);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 14, expectedQuality: 31);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_2_when_sell_by_date_is_between_5_and_10()
        {
            IList<Item> Items = CreateItems(BACKSTAGE_ITEMNAME, 10, 30);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 9, expectedQuality: 32);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_3_when_sell_by_date_is_less_than_5()
        {
            IList<Item> Items = CreateItems(BACKSTAGE_ITEMNAME, 5, 30);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 4, expectedQuality: 33);
        }

        [Fact]
        public void backstage_passes_quality_increase_by_3_when_sell_by_date_is_1()
        {
            IList<Item> Items = CreateItems(BACKSTAGE_ITEMNAME, 1, 30);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 0, expectedQuality: 33);
        }

        [Fact]
        public void backstage_passes_quality_is_0_when_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(BACKSTAGE_ITEMNAME, 0, 30);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 0);
        }

        [Fact]
        public void conjured_quality_decrease_by_two_before_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(CONJURED_ITEMNAME, 3, 5);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: 2, expectedQuality: 3);
        }

        [Fact]
        public void conjured_quality_decrease_by_four_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(CONJURED_ITEMNAME, 0, 5);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 1);
        }

        [Fact]
        public void conjured_with_0_quality_is_still_0_after_sell_by_date_has_passed()
        {
            IList<Item> Items = CreateItems(CONJURED_ITEMNAME, 0, 0);
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            AssertItem(Items[0], expectedSellIn: -1, expectedQuality: 0);
        }


        private static IList<Item> CreateItems(string name, int sellIn, int quality)
        {
            return new List<Item> { new Item { Name = name, SellIn = sellIn, Quality = quality } };
        }

        private static void AssertItem(Item item, int expectedSellIn, int expectedQuality)
        {
            Assert.Equal(expectedSellIn, item.SellIn);
            Assert.Equal(expectedQuality, item.Quality);
        }
    }
}

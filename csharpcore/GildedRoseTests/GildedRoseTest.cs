using Xunit;
using System.Collections.Generic;
using GildedRoseKata;
using Should.Fluent;

namespace GildedRoseTests
{
    public class GildedRoseTest
    {
        const string AgedBrie = "Aged Brie";
        const string BackstagePass = "Backstage passes to a TAFKAL80ETC concert";
        const string Sulfuras = "Sulfuras, Hand of Ragnaros";
        const string Vest = "+5 Dexterity Vest";

        [Fact]
        public void when_updatequality_is_called_sellin_and_quality_are_decreased()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Vest, SellIn = 2, Quality = 10 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.SellIn.Should().Equal(1);
            sut.Quality.Should().Equal(9);
        }

        [Fact]
        public void when_sellin_is_negative_for_all_items_except_aged_brie_quality_is_decreased_twice_as_fast()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Vest, SellIn = -1, Quality = 10 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(8);
        }

        [Fact]
        public void when_sellin_is_positive_quality_is_decreased()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Vest, SellIn = 10, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(19);
        }

        [Fact]
        public void when_sellin_is_changed_quality_is_decreased_but_never_negative()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Vest, SellIn = 10, Quality = 0 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(0);
        }

        [Fact]
        public void when_sellin_is_decreased_for_aged_brie_quality_is_increased_by_one()
        {
            IList<Item> Items = new List<Item> { new Item { Name = AgedBrie, SellIn = 1, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(21);
        }

        [Fact]
        public void when_sellin_has_passed_for_aged_brie_quality_is_increased_by_two()
        {
            IList<Item> Items = new List<Item> { new Item { Name = AgedBrie, SellIn = 0, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(22);
        }

        [Theory]
        [InlineData(AgedBrie, 8, 50, 50)]
        [InlineData(BackstagePass, 8, 50, 50)]
        [InlineData(Sulfuras, 8, 50, 50)]
        public void when_sellin_is_changed_quality_is_never_greater_than_50(string product, int sellIn, int quality, int expectedQuality)
        {
            IList<Item> Items = new List<Item> { new Item { Name = product, SellIn = sellIn, Quality = quality } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(expectedQuality);
        }

        [Fact]
        public void when_sulfuras_quality_never_decreases()
        {
            IList<Item> Items = new List<Item> { new Item { Name = Sulfuras, SellIn = 8, Quality = 80 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(80);

            sut.SellIn = 1;
            app.UpdateQuality();
            sut.Quality.Should().Equal(80);
        }


        [Fact]
        public void when_backstage_pass_and_sellin_less_than_10_days_but_greater_than_5_days_increase_quality_by_2()
        {
            IList<Item> Items = new List<Item> { new Item { Name = BackstagePass, SellIn = 8, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(22);
        }
        
        [Fact]
        public void when_backstage_pass_and_sellin_less_than_5_days_but_greater_than_0_increase_quality_by_3()
        {
            IList<Item> Items = new List<Item> { new Item { Name = BackstagePass, SellIn = 4, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(23);
        }

        [Fact]
        public void when_backstage_pass_and_sellin_is_past_quality_is_zero()
        {
            IList<Item> Items = new List<Item> { new Item { Name = BackstagePass, SellIn = 0, Quality = 20 } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            var sut = Items[0];
            sut.Quality.Should().Equal(0);
        }
    }
}

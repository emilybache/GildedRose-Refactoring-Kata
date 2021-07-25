using Xunit;
using System.Linq;
using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRoseTest
    {
        [Fact]
        public void Once_the_sell_in_date_has_passed_quality_degrades_twice_as_fast()
        {
            var sut = new GildedRose(new List<Item>
            {
                new Item { Name = "Foo", SellIn = 0, Quality = 10 }
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x.Name == "Foo");

            Assert.Equal(8, item.Quality);
        }

        [Fact]
        public void While_the_sell_in_date_has_not_passed_Quality_degrades_normally()
        {
            var sut = new GildedRose(new List<Item>
            {
                new Item { Name = "Foo", SellIn = 1, Quality = 10 }
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x.Name == "Foo");

            Assert.Equal(9, item.Quality);
        }

        [Fact]
        public void The_quality_of_an_item_is_never_negative()
        {
            var sut = new GildedRose(new List<Item>
            {
                new Item { Name = "Foo", SellIn = 10, Quality = 0 }
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x.Name == "Foo");

            Assert.Equal(0, item.Quality);
        }

        [Fact]
        public void Aged_Brie_actually_increases_in_quality_the_older_it_gets()
        {
            var sut = new GildedRose(new List<Item>
            {
                new AgedBrieItem { SellIn = 10, Quality = 5 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x.Name == "Aged Brie");

            Assert.Equal(6, item.Quality);
        }

        [Fact]
        public void The_quality_of_an_item_is_never_more_than_50()
        {
            var sut = new GildedRose(new List<Item>
            {
                new AgedBrieItem { SellIn = 10, Quality = 50 },
                new BackstagePassesItem { SellIn = 10, Quality = 50 },
            });

            sut.UpdateQuality();
            var item1 = sut.Items.First(x => x is AgedBrieItem);
            var item2 = sut.Items.First(x => x is BackstagePassesItem);

            Assert.Equal(50, item1.Quality);
            Assert.Equal(50, item2.Quality);
        }

        [Fact]
        public void Sulfuras_never_has_to_be_sold()
        {
            var sut = new GildedRose(new List<Item>
            {
                new SulfurasItem { SellIn = 10, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x is SulfurasItem);

            Assert.Equal(10, item.SellIn);
        }

        [Fact]
        public void Sulfuras_never_decreases_in_quality()
        {
            var sut = new GildedRose(new List<Item>
            {
                new SulfurasItem { SellIn = 10, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x is SulfurasItem);

            Assert.Equal(20, item.Quality);
        }

        [Fact]
        public void Backstage_passes_increases_in_quality_by_2_when_SellIn_is_10_days_or_less()
        {
            var sut = new GildedRose(new List<Item>
            {
                new BackstagePassesItem { SellIn = 10, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x is BackstagePassesItem);

            Assert.Equal(22, item.Quality);
        }

        [Fact]
        public void Backstage_passes_increases_in_Quality_by_3_when_SellIn_is_5_days_or_less()
        {
            var sut = new GildedRose(new List<Item>
            {
                new BackstagePassesItem { SellIn = 5, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x is BackstagePassesItem);

            Assert.Equal(23, item.Quality);
        }

        [Fact]
        public void Backstage_passes_quality_drops_to_0_after_the_concert()
        {
            var sut = new GildedRose(new List<Item>
            {
                new BackstagePassesItem { SellIn = 0, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x is BackstagePassesItem);

            Assert.Equal(0, item.Quality);
        }
        
        [Fact]
        public void Conjured_items_degrade_twice_as_fast_as_normal_items()
        {
            var sut = new GildedRose(new List<Item>
            {
                new ConjuredItem { Name = "Foo", SellIn = 10, Quality = 20 },
                new ConjuredItem { Name = "Bar", SellIn = 0, Quality = 20 },
            });

            sut.UpdateQuality();
            var item = sut.Items.First(x => x.Name == "Foo");
            var item2 = sut.Items.First(x => x.Name == "Bar");

            Assert.Equal(18, item.Quality);
            Assert.Equal(16, item2.Quality);
        }
    }
}
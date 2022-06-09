using Xunit;
using System.Collections.Generic;
using GildedRoseKata;
using System;
using System.Linq;

namespace GildedRoseTests
{
    public class GildedRoseTest
    {
        [Fact]
        public void foo()
        {
            IList<Item> Items = new List<Item> { new Item { Id = 100, Name = "foo", SellBy = DateTime.Today.AddDays(10), Quality = 20, ProcessedOn = DateTime.Today } };
            GildedRose app = new GildedRose(Items);
            app.UpdateQuality();
            Assert.Equal("foo", Items[0].Name);

            DateTime yesterday = DateTime.Today.AddDays(-1);
            Items = new List<Item>{
                new Item {Id= 0, Name = "+5 Dexterity Vest", SellBy = DateTime.Today.AddDays(10), Quality = 20, ProcessedOn = yesterday},
                new Item {Id= 1, Name = "Aged Brie", SellBy = DateTime.Today.AddDays(2), Quality = 0, ProcessedOn = yesterday},
                new Item {Id= 2, Name = "Elixir of the Mongoose", SellBy = DateTime.Today.AddDays(5), Quality = 7, ProcessedOn = yesterday},
                new Item {Id= 3, Name = "Sulfuras, Hand of Ragnaros", SellBy = DateTime.Today.AddDays(0), Quality = 80, ProcessedOn = yesterday},
                new Item {Id= 4, Name = "Sulfuras, Eye of Ragnaros", SellBy = DateTime.Today.AddDays(-1), Quality = 80, ProcessedOn = yesterday},
                new Item
                {
                    Id= 5,
                    Name = "Backstage pass for Jason Isbell concert",
                    SellBy = DateTime.Today.AddDays(15),
                    Quality = 20,
                    ProcessedOn = yesterday
                },
                new Item
                {
                    Id= 6,
                    Name = "Backstage pass for Lucero concert",
                    SellBy = DateTime.Today.AddDays(10),
                    Quality = 49,
                    ProcessedOn = yesterday
                },
                new Item
                {
                    Id = 7,
                    Name = "Backstage pass Turnpike Troubadours concert",
                    SellBy = DateTime.Today.AddDays(5),
                    Quality = 39,
                    ProcessedOn = yesterday
                },
                // this conjured item does not work properly yet
                new Item {Id = 8, Name = "Conjured Mana Cake", SellBy = DateTime.Today.AddDays(3), Quality = 6, ProcessedOn = yesterday}
            };

            app = new GildedRose(Items);
            app.UpdateQuality();
            var item = Items.Where(c => c.Id == 0).FirstOrDefault();
            Assert.Equal("+5 Dexterity Vest", item.Name);
            Assert.Equal(19, item.Quality);

            item = Items.Where(c => c.Id == 1).FirstOrDefault();
            Assert.Equal(1, item.Quality);

            item = Items.Where(c => c.Id == 2).FirstOrDefault();
            Assert.Equal(6, item.Quality);

            item = Items.Where(c => c.Id == 3).FirstOrDefault();
            Assert.Equal(80, item.Quality);

            item = Items.Where(c => c.Id == 4).FirstOrDefault();
            Assert.Equal(80, item.Quality);

            item = Items.Where(c => c.Id == 5).FirstOrDefault();
            Assert.Equal(19, item.Quality);

            item = Items.Where(c => c.Id == 6).FirstOrDefault();
            Assert.Equal(50, item.Quality);

            item = Items.Where(c => c.Id == 7).FirstOrDefault();
            Assert.Equal(42, item.Quality);

            item = Items.Where(c => c.Id == 8).FirstOrDefault();
            Assert.Equal(5, item.Quality);
        }



    }
}

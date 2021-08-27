using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using GildedRose.Models;

namespace GildedRoseKata
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("OMGHAI!");

            IList<Item> items = new List<Item>{
                new Dexterity() {Name = "+5 Dexterity Vest", SellIn = 10, Quality = 20},
                new AgedBrie() {Name = "Aged Brie", SellIn = 2, Quality = 0},
                new Elixir() {Name = "Elixir of the Mongoose", SellIn = 5, Quality = 7},
                new Sulfuras() {Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80},
                new Sulfuras {Name = "Sulfuras, Hand of Ragnaros", SellIn = -1, Quality = 80},
                new BackStagePasses()
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 15,
                    Quality = 20
                },
                new BackStagePasses
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 10,
                    Quality = 49
                },
                new BackStagePasses
                {
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 5,
                    Quality = 49
                },
				// this conjured item does not work properly yet
				new Conjured() {Name = "Conjured Mana Cake", SellIn = 3, Quality = 6}
            };
            
            var app = new GildedRose(items);
            
            for (var i = 0; i < 31; i++)
            {
                Console.WriteLine("-------- day " + i + " --------");
                Console.WriteLine("name, sellIn, quality");

                foreach (Item t in items)
                {
                    Console.WriteLine(t.Name + ", " + t.SellIn + ", " + t.Quality);
                    Type type = t.GetType();

                    object[] parametersArray = new object[] { "Hello" };

                    var singleMethod = type.GetMethods(BindingFlags.Public)
                        .FirstOrDefault(m => m.Name == "UpdateQuality");

                    singleMethod.Invoke(type, parametersArray);

                }

                Console.WriteLine("");
                app.UpdateQuality();
            }
        }
    }
}

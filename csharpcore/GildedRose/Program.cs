using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Reflection;
using GildedRose.Models;
using Microsoft.VisualBasic.CompilerServices;

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

                    // Get the type contained 
                    Type type = t.GetType();
                    dynamic instance = Activator.CreateInstance(type) as Item;

                    // Get a property on the type that is stored in the
                    // property string
                    PropertyInfo propSellDaysGone = type.GetProperty("SellDaysGone");
                    PropertyInfo propSellIn = type.GetProperty("SellIn");
                    PropertyInfo propQuality = type.GetProperty("Quality");
                    PropertyInfo propName = type.GetProperty("Name");

                    // Set the value of the given property on the given instance
                    propSellDaysGone.SetValue(instance, i, null);
                    propSellIn.SetValue(instance, t.SellIn, null);
                    propQuality.SetValue(instance, t.Quality, null);
                    propName.SetValue(instance, t.Name, null);

                    // Fetch the methods
                    MethodInfo updateQualityMethod = type.GetMethod("UpdateQuality");
                    MethodInfo updateSellinMethod = type.GetMethod("UpdateSellIn");

                    // Invoke the respective implementation of the methods
                    updateQualityMethod.Invoke(instance, new object[0]);
                    updateSellinMethod.Invoke(instance, new object[0]);

                    // update t
                    t.SellIn = instance.SellIn;
                    t.Quality = instance.Quality;

                    Console.WriteLine("");

                }

                Console.WriteLine("");
                //app.UpdateQuality();
            }
        }
    }
}

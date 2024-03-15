using System;
using System.Collections.Generic;
using GildedRoseKata;

namespace GildedRoseTests;

public static class TextTestFixture
{
    public static void Main(string[] args)
    {
        Console.WriteLine("OMGHAI!");

        var items = new List<Item>{
            new ItemBuilder("+5 Dexterity Vest", 10,20).Build(),
            new ItemBuilder("Aged Brie", 2, 0).Build(),
            new ItemBuilder("Elixir of the Mongoose", 5, 7).Build(),
            new LegendaryItemBuilder("Sulfuras, Hand of Ragnaros", 0).Build(),
            new LegendaryItemBuilder("Sulfuras, Hand of Ragnaros", -1).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert",
                15,
                20
                ).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert",
                10,
                49
                ).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert",
                5,
                49
                ).Build(),
            // this conjured item does not work properly yet
            new ItemBuilder("Conjured Mana Cake", 3, 6).Build()
        };

        var app = new GildedRose(items);

        int days = 2;
        if (args.Length > 0)
        {
            days = int.Parse(args[0]) + 1;
        }

        for (var i = 0; i < days; i++)
        {
            Console.WriteLine("-------- day " + i + " --------");
            Console.WriteLine("name, sellIn, quality");
            for (var j = 0; j < items.Count; j++)
            {
                Console.WriteLine(items[j].Name + ", " + items[j].SellIn + ", " + items[j].Quality);
            }
            Console.WriteLine("");
            app.UpdateQuality();
        }
    }
}
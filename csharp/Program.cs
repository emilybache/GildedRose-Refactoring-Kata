using csharp.StrategyPatternExample;
using System;
using System.Collections.Generic;

namespace csharp
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("OMGHAI!");
            
            IList<Item> Items = new List<Item>{
                new Item {Name = Global.NAME_ITEM_PLUS5_DEXTERITY, SellIn = 10, Quality = 20},
                new Item {Name = Global.NAME_ITEM_AGED_BRIE, SellIn = 2, Quality = 0},
                new Item {Name = Global.NAME_ITEM_ELIXIR_MONGOOSE, SellIn = 5, Quality = 7},
                new Item {Name = Global.NAME_ITEM_SULFURAS, SellIn = 0, Quality = 80},
                new Item {Name = Global.NAME_ITEM_SULFURAS, SellIn = -1, Quality = 80},
                new Item
                {
                    Name = Global.NAME_ITEM_BACKSTAGE_PASSES,
                    SellIn = 15,
                    Quality = 20
                },
                new Item
                {
                    Name = Global.NAME_ITEM_BACKSTAGE_PASSES,
                    SellIn = 10,
                    Quality = 49
                },
                new Item
                {
                    Name = Global.NAME_ITEM_BACKSTAGE_PASSES,
                    SellIn = 5,
                    Quality = 49
                },
				new Item {Name = Global.NAME_ITEM_CONJURED, SellIn = 3, Quality = 6}
            };
            
            IGildedRoseApp app = null;

            if ((args.Length > 0) && (args[0] == typeof(GildedRoseStrategyPatternExample).Name))
            {
                app = new GildedRoseStrategyPatternExample(Items);
            }
            else
            {
                app = new GildedRose(Items);
            }

            for (var i = 0; i < 31; i++)
            {
                Console.WriteLine("-------- day " + i + " --------");
                Console.WriteLine("name, sellIn, quality");
                for (var j = 0; j < Items.Count; j++)
                {
                    System.Console.WriteLine(Items[j].Name + ", " + Items[j].SellIn + ", " + Items[j].Quality);
                }
                Console.WriteLine("");
                app.UpdateQuality();
            }
        }
    }
}

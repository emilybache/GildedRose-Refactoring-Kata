using System;
using System.Collections.Generic;

namespace csharp
{
    interface IUpdateMethods
    {
        void CalculateQuality(Item item, int decrementValue, int incrementValue);
        void AgedBrie(Item item);
        void MinMaxRules(Item item);
        void ReduceSellIn(Item item);
    }

    public class GildedRose : IUpdateMethods
    {
        IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }

        enum Limit
        {
            RegularMin = 0,
            RegularMax = 50,
            LegendaryMax = 80
        }

        public void UpdateQuality()
        {
            foreach(var Item in Items) // Changed this to a simpler loop
            {
                //Console.WriteLine(Item.Name);
                if(Item.Name == "Aged Brie")
                {
                    AgedBrie(Item);
                    MinMaxRules(Item);
                    //return;
                }
                else if (Item.Name == "Sulfuras, Hand of Ragnaros")
                {
                    //CalculateQuality(Item, 0, 0);
                    //return;
                }
                else if(Item.Name == "Conjured Mana Cake")
                {
                    CalculateQuality(Item, 4, 0);
                    //return;
                }
                else 
                {
                    CalculateQuality(Item, 2, 0);
                    //return;
                }
            }
        }

        public void CalculateQuality(Item item, int decrementValue, int IncrementValue)
        {
            if (item.SellIn <= 50)
            {
                item.Quality = item.Quality - decrementValue;
            }
            MinMaxRules(item);
            ReduceSellIn(item);
        }
        public void AgedBrie(Item item)
        {
            if (item.SellIn == 0)
            {
                item.Quality = 0;
            }

            if (item.SellIn <= 10 && item.SellIn > 5 && item.SellIn > 0)
            {
                item.Quality = item.Quality + 2;
            }

            if (item.SellIn <= 5 && item.SellIn > 0)
            {
                item.Quality = item.Quality + 3;
            }
            ReduceSellIn(item);
        }

        public void MinMaxRules(Item item)
        {
            if(item.Quality <= (int)Limit.RegularMin)
            {
                item.Quality = (int)Limit.RegularMin;
            }
            if(item.Quality >= (int)Limit.RegularMax)
            {
                item.Quality = (int)Limit.RegularMax;
            }
        }

        public void ReduceSellIn(Item item)
        {
            if (item.SellIn >= 1)
            {
                item.SellIn = item.SellIn -  1;
            }
        }

    }
}

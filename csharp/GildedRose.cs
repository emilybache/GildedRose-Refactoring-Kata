using System;
using System.Collections.Generic;

namespace csharp
{
    interface IUpdateInterface
    {
        void CalculateItemQuality(Item item, int decrementValue, int incrementValue); //takes 3 self explanatory params.
        void CalculateSpecialItemQuality(Item item); // Aged Brie Method
        void MinMaxRules(Item item); // Minimum and Maximum Rules for item quality
        void ReduceSellIn(Item item); // Reduce number of days left method
    }

    public class GildedRose : IUpdateInterface
    {
        IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }

        public void UpdateQuality()
        {
            foreach (var Item in Items) // Changed this to a simpler loop
            {
                //Console.WriteLine(Item.Name);
                switch (Categorize(Item.Name))
                {
                    case 1:
                        CalculateSpecialItemQuality(Item);
                        break;
                    case 2:
                        CalculateLegendaryItemQuality
                            (Item, 0, 0);
                        break;
                    case 3:
                        CalculateItemQuality(Item, 4, 0);
                        break;
                    default:
                        CalculateItemQuality(Item, 2, 0);
                        break;
                }
                //if (Item.Name == "Aged Brie")
                //{
                //    CalculateSpecialItemQuality(Item);
                //    //MinMaxRules(Item);
                //    //return;
                //}
                //else if (Item.Name == "Sulfuras, Hand of Ragnaros")
                //{
                //    //CalculateQuality(Item, 0, 0);
                //    //return;
                //}
                //else if (Item.Name == "Conjured Mana Cake")
                //{
                //    CalculateStandardItemQuality(Item, 4, 0);
                //    //return;
                //}
                //else
                //{
                //    CalculateStandardItemQuality(Item, 2, 0);
                //    //return;
                //}
            }
        }

        public void CalculateItemQuality(Item item, int decrementValue, int IncrementValue) // To calculate the quality of each item
        {
            //if (item.SellIn > 0 )
            {
                item.Quality = item.Quality - decrementValue;
            }
            MinMaxRules(item);
            ReduceSellIn(item);
        }
        public void CalculateLegendaryItemQuality(Item item, int decrementValue, int IncrementValue) // To calculate the quality of each item
        {
            ////if (item.SellIn > 0 )
            //{
            //    item.Quality = item.Quality - decrementValue;
            //}
            //MinMaxRules(item);
            ReduceSellIn(item);
        }
        public void CalculateSpecialItemQuality(Item item) // Specific for the AgedBrie Special Item
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
            MinMaxRules(item);
        }

        public void MinMaxRules(Item item) //Minimum and Maximum Rules
        {
            if (item.Quality <= 0)
            {
                item.Quality = 0;
            }
            if (item.Quality >= 50)
            {
                item.Quality = 50;
            }
        }

        public void ReduceSellIn(Item item) //This will take care of reducing the number of days after each  day
        {
            //if (item.SellIn >= 1)
            {
                item.SellIn = item.SellIn - 1;
            }
        }

        // Here is an attempt to categorize the items since am not allowed to touch the items class
        public int Categorize(String itemName) //This will take care of reducing the number of days after each  day
        {
            int categoryID;
            switch (itemName)
            {
                case string n when n.Contains("Aged Brie"):
                    categoryID = 1;
                    break;
                case string n when n.Contains("Sulfuras"):
                    categoryID = 2;
                    break;
                case string n when n.Contains("Conjured"):
                    categoryID = 3;
                    break;
                default:
                    categoryID = 4;
                    break;
            }
            return categoryID;
        }

    }
}

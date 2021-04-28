using System;
using System.Collections.Generic;

namespace csharp
{
    #region Interface
    interface IUpdateInterface
    {
        void CalculateItemQuality(Item item, int decrementValue, int incrementValue); //takes 3 self explanatory params.
        void CalculateSpecialItemQuality(Item item); // Aged Brie Method
        void CalculateLegendaryItemQuality(Item item); // Calculate legendary item
        void MinMaxRules(Item item); // Minimum and Maximum Rules for item quality
        void ReduceSellIn(Item item); // Reduce number of days left method
    }
    #endregion

    public class GildedRose : IUpdateInterface
    {
        #region constructor
        private IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }
        #endregion

        #region Update Quality
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
                        CalculateLegendaryItemQuality(Item);
                        break;
                    case 3:
                        CalculateItemQuality(Item, 4, 0);
                        break;
                    default:
                        CalculateItemQuality(Item, 2, 0);
                        break;
                }
            }
        }
        #endregion

        #region Calculate Item Quality
        public void CalculateItemQuality(Item item, int decrementValue, int IncrementValue) // To calculate the quality of each item
        {
            item.Quality -= decrementValue;
            MinMaxRules(item);
            ReduceSellIn(item);
        }
        #endregion

        #region Calculate LEgendary Item Quality
        public void CalculateLegendaryItemQuality(Item item) // To calculate the quality of each item
        {
            ReduceSellIn(item);
        }
        #endregion

        #region Calculate Special Item Quality
        public void CalculateSpecialItemQuality(Item item) // Specific for the AgedBrie Special Item
        {
            item.Quality = item.SellIn == 0 ? 0 : item.Quality;
            item.Quality = (item.SellIn <= 10 && item.SellIn > 5 && item.SellIn > 0) ? item.Quality + 2 : item.Quality;
            item.Quality = (item.SellIn <= 5 && item.SellIn > 0) ? item.Quality + 3 : item.Quality;
            
            ReduceSellIn(item);
            MinMaxRules(item);
        }
        #endregion

        #region Minimum and Maximum Rules
        public void MinMaxRules(Item item) //Minimum and Maximum Rules
        {
            item.Quality = (item.Quality <= 0) ? 0 : item.Quality;
            item.Quality = (item.Quality >= 50) ? 50 : item.Quality;
        }
        #endregion

        #region Reduce Sell In 
        public void ReduceSellIn(Item item) //This will take care of reducing the number of days after each  day
        {
            item.SellIn -= 1;
        }
        #endregion

        #region Categorize Items
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
        #endregion
    }
}

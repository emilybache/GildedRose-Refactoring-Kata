using System.Collections.Generic;
using System;
namespace csharp
{
    public class GildedRose
    {
        IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }
        void UpdateItem(ref Item item, int sellInValueFactor, int qualityValueFactor)
        {
            item.SellIn += sellInValueFactor;
            item.Quality =  Math.Max(0, Math.Min(50, item.Quality + qualityValueFactor));

        }
        bool IsExpired(Item item)
        {
            bool output = true;
            if (item.SellIn > 0)
                output = false;
            return output;
        }
        Item GetNewAgedBrie(Item item)
        {
            if (IsExpired(item))
                UpdateItem(ref item, -1, 2);
            else
                UpdateItem(ref item, -1, 1);
            return item;
        }
        Item GetNewBackstagesPasses(Item item)
        {
            if (IsExpired(item))
                item.Quality = 0;
            else
            {
                if (item.SellIn <= 10 && item.SellIn > 5)
                {
                    UpdateItem(ref item, -1, 2);
                }
                else if (item.SellIn <= 5)
                {
                    UpdateItem(ref item, -1, 3);
                }
                else
                {
                    UpdateItem(ref item, -1, 1);
                }
            }
            return item;
        }
        Item GetNewConjured(Item item)
        {
            if (IsExpired(item))
            {
                UpdateItem(ref item, -1, -4);
            }
            else
            {
                UpdateItem(ref item, -1, -2);
            }
            return item;
        }
        Item GetNewDefault(Item item)
        {
            if (IsExpired(item))
            {
                UpdateItem(ref item, -1, -2);
            }
            else
            {
                UpdateItem(ref item, -1, -1);
            }
            return item;
        }
        public void UpdateQuality()
        {
            for (int itemIndex = 0; itemIndex < Items.Count; itemIndex++)
            {
                switch (Items[itemIndex].Name)
                {
                    case "Aged Brie":
                        Items[itemIndex] = GetNewAgedBrie(Items[itemIndex]);
                        break;
                    case "Sulfuras, Hand of Ragnaros":
                        break;
                    case "Backstage passes to a TAFKAL80ETC concert":
                        Items[itemIndex] = GetNewBackstagesPasses(Items[itemIndex]);
                        break;
                    case "Conjured Mana Cake":
                        Items[itemIndex] = GetNewConjured(Items[itemIndex]);
                        break;
                    default:
                        Items[itemIndex] = GetNewDefault(Items[itemIndex]);
                        break;
                }
            }
        }

    }
}

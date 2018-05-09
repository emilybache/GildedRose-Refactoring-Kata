using System;
using System.Collections.Generic;
using System.Runtime.Remoting.Messaging;

namespace csharp
{
    public class GildedRose
    {

        private readonly IList<Item> _items;

        public GildedRose(IList<Item> items)
            => _items = items;

        private static int GetQualityChange(Item item)
        {
            var sellIn = item.SellIn;

            switch (item.Name)
            {
                case "Sulfuras, Hand of Ragnaros":
                    return 0;
                case "Backstage passes to a TAFKAL80ETC concert":
                    if (sellIn > 9)
                    {
                        return 1;
                    }
                    else if (sellIn > 4)
                    {
                        return 2;
                    }
                    else if (sellIn >= 0)
                    {
                        return 3;
                    }
                    else
                    {
                        return -item.Quality;
                    }
                case "Aged Brie":
                    return sellIn < 0 ? 2 : 1;
                case "Conjured Mana Cake":
                    return sellIn < 0 ? -4 : -2;
                default:
                    return sellIn < 0 ? -2 : -1;
            }
        }

        public void UpdateQuality()
        {
            foreach (var item in _items)
            {
                item.SellIn--;

                var qualityChange = GetQualityChange(item);

                item.Quality += qualityChange;

                if (item.Name != "Sulfuras, Hand of Ragnaros")
                {
                    item.Quality = Math.Min(50, item.Quality);
                }
                else
                {
                    item.SellIn++;
                }

                item.Quality = Math.Max(0, item.Quality);
            }
        }
    }
}

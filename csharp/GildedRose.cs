using System.Collections.Generic;

namespace csharp
{
    public class GildedRose
    {
        private readonly IList<Item> _items;
        
        public GildedRose(IList<Item> items)
        {
            _items = items;
        }

        public void UpdateQuality()
        {
            foreach (var item in _items)
            {
                UpdateItemQuality(item);
                UpdateItemSellIn(item);
            }
        }
        
        private void UpdateItemQuality(Item item)
        {
            if (item.Name == "Aged Brie")
            {
                IncreaseQuality(item);
                if (item.SellIn < 0)
                {
                    IncreaseQuality(item);
                }
            }
            else if (item.Name == "Backstage passes to a TAFKAL80ETC concert")
            {
                IncreaseQuality(item);
                if (item.SellIn < 11)
                {
                    IncreaseQuality(item);
                }
                if (item.SellIn < 6)
                {
                    IncreaseQuality(item);
                }
                if (item.SellIn < 0)
                {
                    item.Quality = 0;
                }
            }
            else if (item.Name == "Sulfuras, Hand of Ragnaros")
            {
                // Do nothing, the item's quality and sell-in never change.
            }
            else
            {
                DecreaseQuality(item);
                if (item.SellIn < 0)
                {
                    DecreaseQuality(item);
                }
            }
        }
        
        private void IncreaseQuality(Item item)
        {
            if (item.Quality < 50)
            {
                item.Quality++;
            }
        }
        
        private void DecreaseQuality(Item item)
        {
            if (item.Quality > 0)
            {
                item.Quality--;
            }
        }
        
        private void UpdateItemSellIn(Item item)
        {
            if (item.Name != "Sulfuras, Hand of Ragnaros")
            {
                item.SellIn--;
            }
        }
    }
}

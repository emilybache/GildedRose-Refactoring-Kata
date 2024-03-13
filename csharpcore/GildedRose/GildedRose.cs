using System.Collections.Generic;

namespace GildedRoseKata;

public class GildedRose
{
    private readonly IList<Item> _items;
    const int MinQuality = 0;
    const int MaxQuality = 50;

    public GildedRose(IList<Item> items)
    {
        _items = items;
    }

    public void UpdateQuality()
    {
        foreach (var item in _items)
        {
            DailyItemUpdate(item);
        }
    }

    private static bool IsLegendaryItem(Item item) => item.Name.ToLower().Contains("sulfuras");

    private static bool IsBackstagePassesItem(Item item) => item.Name.ToLower().Contains("backstage passes");

    private static bool IsBetterWithAgeItem(Item item) => item.Name.ToLower().Equals("aged brie");

    private static bool IsRegularItem(Item item) => (!IsLegendaryItem(item) &&
                                                     !IsBackstagePassesItem(item) &&
                                                     !IsBetterWithAgeItem(item));


    private static void IncreaseQuality(Item item, int byValue)
    {
        item.Quality = int.Min(item.Quality + byValue, MaxQuality);
    }
    
    private static void DecreaseQuality(Item item, int byValue)
    {
        item.Quality = int.Max(item.Quality - byValue, MinQuality);
    }


    private void DailyItemUpdate(Item item)
    {
        if (IsLegendaryItem(item)) return;
        
        if (IsRegularItem(item))
        {
            DecreaseQuality(item, 1);
        }
        
        if(IsBetterWithAgeItem(item) || IsBackstagePassesItem(item))
        {
            IncreaseQuality(item, 1);

            if (IsBackstagePassesItem(item))
            {
                if (item.SellIn < 11)
                {
                    IncreaseQuality(item, 1);
                }

                if (item.SellIn < 6)
                {
                    IncreaseQuality(item, 1);
                }
            }
        }

        item.SellIn = item.SellIn - 1;
        
        if (item.SellIn < 0)
        {
            if (IsRegularItem(item))
            {
                DecreaseQuality(item, 1);
            }
            
            if(IsBackstagePassesItem(item))
            {
                DecreaseQuality(item, item.Quality);
            }

            if(IsBetterWithAgeItem(item))
            {
                IncreaseQuality(item, 1);
            }
        }
    }
}
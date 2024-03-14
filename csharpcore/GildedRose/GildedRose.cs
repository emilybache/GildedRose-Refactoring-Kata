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
            DailyUpdater dailyUpdater = getDailyUpdater(item);
            dailyUpdater.DailyUpdate(item);
        }
    }

    private static bool IsLegendaryItem(Item item) => item.Name.ToLower().Contains("sulfuras");

    private static bool IsBackstagePassesItem(Item item) => item.Name.ToLower().Contains("backstage passes");

    private static bool IsBetterWithAgeItem(Item item) => item.Name.ToLower().Equals("aged brie");
    
    private DailyUpdater getDailyUpdater(Item item)
    {
        if (IsLegendaryItem(item))
        {
            return new DailyUpdaterForLegendaryItems();
        }

        if (IsBetterWithAgeItem(item))
        {
            return new DailyUpdaterForBetterWithAgeItems();
        }

        if (IsBackstagePassesItem(item))
        {
            return new DailyUpdaterForBackstagePassesItems();
        }

        return new DailyUpdaterForRegularItems();
    }
}
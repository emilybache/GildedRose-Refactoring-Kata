using System;
using System.Collections.Generic;

namespace GildedRoseKata;

public class DailyUpdaterFactory
{
    private enum ItemType
    {
        Regular,
        Legendary,
        BetterWithAge,
        BackstagePasses
    }

    private readonly Dictionary<ItemType, DailyUpdater> _dailyUpdaters = new();
    
    public DailyUpdater GetDailyUpdater(Item item)
    {
        if (IsLegendaryItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.Legendary, () => new DailyUpdaterForLegendaryItems());
        } 
        
        if (IsBetterWithAgeItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.BetterWithAge, () => new DailyUpdaterForBetterWithAgeItems());
        }

        if(IsBackstagePassesItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.BackstagePasses, () => new DailyUpdaterForBackstagePassesItems());
        }

        return GetOrCreateDailyUpdater(ItemType.Regular, () => new DailyUpdaterForRegularItems());
    }

    private DailyUpdater GetOrCreateDailyUpdater(ItemType itemType, Func<DailyUpdater> createDailyUpdater)
    {
        if (!_dailyUpdaters.ContainsKey(itemType))
        {
            _dailyUpdaters.Add(itemType, createDailyUpdater());
        }

        return _dailyUpdaters[itemType];
    }
    
    private static bool IsLegendaryItem(Item item) => item.Name.ToLower().Contains("sulfuras");

    private static bool IsBackstagePassesItem(Item item) => item.Name.ToLower().Contains("backstage passes");

    private static bool IsBetterWithAgeItem(Item item) => item.Name.ToLower().Equals("aged brie");
    
}
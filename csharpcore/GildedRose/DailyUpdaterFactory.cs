using System;
using System.Collections.Generic;

namespace GildedRoseKata;

public class DailyUpdaterFactory
{
    
    private readonly Dictionary<ItemType.ItemKey, DailyUpdater> _dailyUpdaters = new();
    
    public DailyUpdater GetDailyUpdater(Item item)
    {
        if (ItemType.IsLegendaryItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.ItemKey.Legendary, () => new DailyUpdaterForLegendaryItems());
        } 
        
        if (ItemType.IsBetterWithAgeItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.ItemKey.BetterWithAge, () => new DailyUpdaterForBetterWithAgeItems());
        }

        if(ItemType.IsBackstagePassesItem(item))
        {
            return GetOrCreateDailyUpdater(ItemType.ItemKey.BackstagePasses, () => new DailyUpdaterForBackstagePassesItems());
        }

        return GetOrCreateDailyUpdater(ItemType.ItemKey.Regular, () => new DailyUpdaterForRegularItems());
    }

    private DailyUpdater GetOrCreateDailyUpdater(ItemType.ItemKey itemType, Func<DailyUpdater> createDailyUpdater)
    {
        if (!_dailyUpdaters.ContainsKey(itemType))
        {
            _dailyUpdaters.Add(itemType, createDailyUpdater());
        }

        return _dailyUpdaters[itemType];
    }
}
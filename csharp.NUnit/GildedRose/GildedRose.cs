﻿using System.Collections.Generic;

namespace GildedRoseKata;

public class GildedRose
{
    private readonly IList<Item> Items;

    public GildedRose(IList<Item> Items)
    {
        this.Items = Items;
    }

    public void UpdateQuality()
    {
        foreach (var item in Items)
        {
            var strategy = UpdateStrategyFactory.CreateStrategy(item.Name);
            strategy.UpdateQuality(item);
        }
    }
}

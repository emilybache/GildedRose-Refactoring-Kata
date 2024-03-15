using System;

namespace GildedRoseKata;

public class LegendaryItemBuilder(string name, int sellIn)
    : AbstractItemBuilder(name, sellIn, ItemQuality.LegendaryItemQuality)
{
    public override Item Build()
    {
        if (!ItemType.IsLegendaryItem(Name))
        {
            throw new ArgumentException("Only Legendary items can be built using this builder");
        }

        return base.Build();
    }
}
using System;

namespace GildedRoseKata;

public class ItemBuilder(string name, int sellIn, int quality) : AbstractItemBuilder(name, sellIn, quality)
{
    public override Item Build()
    {
        if (Quality < ItemQuality.MinQuality || Quality > ItemQuality.MaxQuality)
        {
            throw new ArgumentException("An item's quality must be >=0 and <=50");
        }

        if (ItemType.IsLegendaryItem(Name))
        {
            throw new ArgumentException("Legendary Items cannot be constructed using this builder");
        }

        return base.Build();
    }
}



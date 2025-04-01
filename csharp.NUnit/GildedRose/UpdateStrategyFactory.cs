namespace GildedRoseKata;

using GildedRoseKata.Strategies;

public static class UpdateStrategyFactory
{
    public static IUpdateStrategy CreateStrategy(string itemName)
    {
        return itemName switch
        {
            ItemCategory.AgedBrie => new AgedBrieStrategy(),
            ItemCategory.BackstagePasses => new BackstagePassStrategy(),
            ItemCategory.Sulfuras => new SulfurasStrategy(),
            ItemCategory.Conjured => new ConjuredItemStrategy(),
            _ => new StandardItemStrategy()
        };
    }
}

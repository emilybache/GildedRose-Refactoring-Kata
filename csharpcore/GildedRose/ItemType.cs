namespace GildedRoseKata;

public static class ItemType
{
    public enum ItemKey
    {
        Regular,
        Legendary,
        BetterWithAge,
        BackstagePasses,
        Conjured
    }
    
    public static bool IsLegendaryItem(Item item) => IsLegendaryItem(item.Name);
    public static bool IsLegendaryItem(string name) => name.ToLower().Contains("sulfuras");

    public static bool IsBackstagePassesItem(Item item) => item.Name.ToLower().Contains("backstage passes");

    public static bool IsBetterWithAgeItem(Item item) => item.Name.ToLower().Equals("aged brie");

    public static bool IsConjuredItem(Item item) => item.Name.ToLower().Contains("conjured");
}
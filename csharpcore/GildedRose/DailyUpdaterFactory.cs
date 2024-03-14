namespace GildedRoseKata;

public static class DailyUpdaterFactory
{
    public static DailyUpdater GetDailyUpdater(Item item)
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
    
    private static bool IsLegendaryItem(Item item) => item.Name.ToLower().Contains("sulfuras");

    private static bool IsBackstagePassesItem(Item item) => item.Name.ToLower().Contains("backstage passes");

    private static bool IsBetterWithAgeItem(Item item) => item.Name.ToLower().Equals("aged brie");
    
}
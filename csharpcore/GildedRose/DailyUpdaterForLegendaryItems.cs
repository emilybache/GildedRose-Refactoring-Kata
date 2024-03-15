namespace GildedRoseKata;

public class DailyUpdaterForLegendaryItems : DailyUpdater
{
    public override void UpdateSellIn(Item item)
    {
        // Legendary Items don't change over time
    }
    public override void UpdateQuality(Item item)
    {
        // Legendary Items don't change over time
    }
}
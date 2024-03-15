namespace GildedRoseKata;

public class DailyUpdaterForBetterWithAgeItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        IncreaseQuality(item);
    }
}
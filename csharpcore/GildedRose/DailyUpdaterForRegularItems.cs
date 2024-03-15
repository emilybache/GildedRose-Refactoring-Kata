namespace GildedRoseKata;

public class DailyUpdaterForRegularItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        DecreaseQuality(item);
    }
}
namespace GildedRoseKata;

public class DailyUpdaterForConjuredItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        DecreaseQuality(item, 2);
    }
}
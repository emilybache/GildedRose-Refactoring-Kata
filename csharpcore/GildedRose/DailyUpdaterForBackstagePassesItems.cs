namespace GildedRoseKata;

public class DailyUpdaterForBackstagePassesItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        if (item.SellIn > 9)
        {
            IncreaseQuality(item);
        }
        else if (item.SellIn > 4)
        {
            IncreaseQuality(item, 2);
        }
        else if (!IsExpired(item))
        {
            IncreaseQuality(item, 3);
        }
        else //Expired
        {
            DecreaseQuality(item, item.Quality);
        }
    }
}
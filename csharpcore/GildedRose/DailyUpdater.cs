using System.Data;

namespace GildedRoseKata;

public abstract class DailyUpdater
{
    protected const int MinQuality = 0;
    protected const int MaxQuality = 50;

    public void DailyUpdate(Item item)
    {
        UpdateSellIn(item);
        UpdateQuality(item);
    }

    public abstract void UpdateQuality(Item item);
    public virtual void UpdateSellIn(Item item) => item.SellIn -= 1;

    protected static bool IsExpired(Item item) => item.SellIn < 0;

    protected static void IncreaseQuality(Item item, int byValue)
    {
        item.Quality = int.Min(item.Quality + byValue, MaxQuality);
    }

    protected static void DecreaseQuality(Item item, int byValue)
    {
        item.Quality = int.Max(item.Quality - byValue, MinQuality);
    }
    
    
}

public class DailyUpdaterForRegularItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        DecreaseQuality(item, 1);
        if (IsExpired(item))
        {
            DecreaseQuality(item, 1);
        }
    }
}

public class DailyUpdaterForBetterWithAgeItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        IncreaseQuality(item, 1);
        if (IsExpired(item))
        {
            IncreaseQuality(item, 1);
        }
    }
}


public class DailyUpdaterForBackstagePassesItems : DailyUpdater
{
    public override void UpdateQuality(Item item)
    {
        if (item.SellIn > 9)
        {
            IncreaseQuality(item, 1);
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



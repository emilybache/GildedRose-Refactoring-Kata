using System.Data;

namespace GildedRoseKata;

public abstract class DailyUpdater
{
    public void DailyUpdate(Item item)
    {
        UpdateSellIn(item);
        UpdateQuality(item);
        if (IsExpired(item))
        {
            UpdateQuality(item);
        }
    }

    public abstract void UpdateQuality(Item item);
    public virtual void UpdateSellIn(Item item) => item.SellIn -= 1;

    protected static bool IsExpired(Item item) => item.SellIn < 0;

    protected static void IncreaseQuality(Item item, int byValue = 1)
    {
        item.Quality = int.Min(item.Quality + byValue, ItemQuality.MaxQuality);
    }

    protected static void DecreaseQuality(Item item, int byValue = 1)
    {
        item.Quality = int.Max(item.Quality - byValue, ItemQuality.MinQuality);
    }
}
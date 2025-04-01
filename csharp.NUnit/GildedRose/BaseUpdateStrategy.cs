namespace GildedRoseKata;

public abstract class BaseUpdateStrategy : IUpdateStrategy
{
    protected static void DecreaseQuality(Item item)
    {
        if (item.Quality > ItemCategory.MinQuality)
        {
            item.Quality--;
        }
    }

    protected static void IncreaseQuality(Item item)
    {
        if (item.Quality < ItemCategory.MaxQuality)
        {
            item.Quality++;
        }
    }

    protected static void DecreaseSellIn(Item item)
    {
        item.SellIn--;
    }

    public abstract void UpdateQuality(Item item);
}

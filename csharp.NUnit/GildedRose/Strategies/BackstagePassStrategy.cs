namespace GildedRoseKata.Strategies;

public class BackstagePassStrategy : BaseUpdateStrategy
{
    public override void UpdateQuality(Item item)
    {
        IncreaseQuality(item);

        if (item.SellIn <= 10)
        {
            IncreaseQuality(item);
        }

        if (item.SellIn <= 5)
        {
            IncreaseQuality(item);
        }

        DecreaseSellIn(item);

        if (item.SellIn < 0)
        {
            item.Quality = ItemCategory.MinQuality;
        }
    }
}

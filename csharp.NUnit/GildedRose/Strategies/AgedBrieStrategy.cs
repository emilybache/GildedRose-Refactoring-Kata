namespace GildedRoseKata.Strategies;

public class AgedBrieStrategy : BaseUpdateStrategy
{
    public override void UpdateQuality(Item item)
    {
        IncreaseQuality(item);
        DecreaseSellIn(item);

        if (item.SellIn < 0)
        {
            IncreaseQuality(item);
        }
    }
}

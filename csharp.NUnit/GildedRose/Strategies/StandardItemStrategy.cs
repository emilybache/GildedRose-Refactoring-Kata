namespace GildedRoseKata.Strategies;

public class StandardItemStrategy : BaseUpdateStrategy
{
    public override void UpdateQuality(Item item)
    {
        DecreaseQuality(item);
        DecreaseSellIn(item);

        if (item.SellIn < 0)
        {
            DecreaseQuality(item);
        }
    }
}

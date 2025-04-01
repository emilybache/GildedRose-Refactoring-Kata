namespace GildedRoseKata.Strategies;

public class ConjuredItemStrategy : BaseUpdateStrategy
{
    public override void UpdateQuality(Item item)
    {
        // For now, Conjured items degrade at normal rate to match original behavior
        DecreaseQuality(item);
        DecreaseSellIn(item);

        if (item.SellIn < 0)
        {
            DecreaseQuality(item);
        }
    }
}

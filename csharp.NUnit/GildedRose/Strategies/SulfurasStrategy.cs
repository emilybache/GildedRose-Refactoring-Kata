namespace GildedRoseKata.Strategies;

public class SulfurasStrategy : BaseUpdateStrategy
{
    public override void UpdateQuality(Item item)
    {
        // Sulfuras is legendary and never changes
        item.Quality = ItemCategory.LegendaryQuality;
    }
}

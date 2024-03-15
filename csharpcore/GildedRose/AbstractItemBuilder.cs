namespace GildedRoseKata;

public abstract class AbstractItemBuilder(string name, int sellIn, int quality)
{
    protected readonly string Name = name;
    protected readonly int SellIn = sellIn;
    protected readonly int Quality = quality;

    public virtual Item Build()
    {
        return new() { Name = Name, SellIn = SellIn, Quality = Quality };
    }
}
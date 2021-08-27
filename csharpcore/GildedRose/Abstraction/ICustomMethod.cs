namespace GildedRose.Abstraction
{
    public interface ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public abstract void UpdateQuality();
        public abstract void UpdateSellIn();

    }
}
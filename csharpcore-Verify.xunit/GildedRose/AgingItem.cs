namespace GildedRoseKata
{
    public class AgingItem : Item
    {

        public virtual void SetAgingItemQuality()
        {
            if (Quality < 50)
            {
                Quality++;

                SellIn--;

                if (SellIn < 0 && Quality < 50)
                {
                    Quality++;
                }
            }
        }
    }
}

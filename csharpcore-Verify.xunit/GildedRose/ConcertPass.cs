using GildedRoseKata;

namespace GildedRoseKata
{
    public class ConcertPass : AgingItem
    {
        public override void SetAgingItemQuality()
        {
            if (Quality < 50)
            {
                Quality++;

                if (SellIn < 11 && Quality < 50)
                {
                    Quality++;
                }

                if (SellIn < 6 && Quality < 50)
                {
                    Quality++;
                }

                SellIn--;
                if (SellIn < 0)
                {
                    Quality = 0;
                }
            }
        }
    }
}

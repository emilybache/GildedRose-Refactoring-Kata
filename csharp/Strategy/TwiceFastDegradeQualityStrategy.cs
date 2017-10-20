using System;

namespace csharp.Strategy
{
    class TwiceFastDegradeQualityStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            int degrade = 1;

            item.SellIn--;

            if (item.SellIn < 0)
            {
                degrade = 2;
            }

            if (item.Quality > 0)
            {
                item.Quality -= degrade;
            }

            if (item.Quality < 0)
            {
                item.Quality = 0;
            }            
        }
    }
}

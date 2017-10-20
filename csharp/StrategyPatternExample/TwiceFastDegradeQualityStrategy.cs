using System;

namespace csharp.StrategyPatternExample
{
    class TwiceFastDegradeQualityStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            int degrade = 2;

            item.SellIn--;

            if (item.SellIn < 0)
            {
                degrade = 4;
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

using System;

namespace csharp.StrategyPatternExample.Strategy
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

            if (item.Quality > Global.MINIMUN_QUALITY)
            {
                item.Quality -= degrade;
            }

            if (item.Quality < Global.MINIMUN_QUALITY)
            {
                item.Quality = Global.MINIMUN_QUALITY;
            }            
        }
    }
}

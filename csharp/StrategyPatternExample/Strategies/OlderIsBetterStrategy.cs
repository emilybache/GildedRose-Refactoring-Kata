using System;

namespace csharp.StrategyPatternExample.Strategy
{
    class OlderIsBetterStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            if (item.Quality < Global.MAXIMUN_QUALITY)
            {
                item.Quality++;
            }

            item.SellIn--;

            if (item.SellIn < 0 && item.Quality < Global.MAXIMUN_QUALITY)
            {
                item.Quality++;
            }
        }
    }
}

using System;

namespace csharp.StrategyPatternExample.Strategy
{
    internal class OlderIsBetterStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            if (item.Quality < Global.MAXIMUM_QUALITY)
            {
                item.Quality++;
            }

            item.SellIn--;

            if (item.SellIn < 0 && item.Quality < Global.MAXIMUM_QUALITY)
            {
                item.Quality++;
            }
        }
    }
}

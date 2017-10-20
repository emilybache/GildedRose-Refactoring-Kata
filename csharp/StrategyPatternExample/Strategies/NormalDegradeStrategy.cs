using System;

namespace csharp.StrategyPatternExample.Strategy
{
    internal class NormalDegradeStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            if (item.Quality > Global.MINIMUM_QUALITY)
            {
                item.Quality--;
            }

            item.SellIn--;

            if (item.SellIn < 0 && item.Quality > Global.MINIMUM_QUALITY)
            {
                item.Quality--;
            }            
        }
    }
}

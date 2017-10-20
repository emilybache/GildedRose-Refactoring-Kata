using System;

namespace csharp.StrategyPatternExample.Strategy
{
    class NormalDegradeStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            if (item.Quality > Global.MINIMUN_QUALITY)
            {
                item.Quality--;
            }

            item.SellIn--;

            if (item.SellIn < 0 && item.Quality > Global.MINIMUN_QUALITY)
            {
                item.Quality--;
            }            
        }
    }
}

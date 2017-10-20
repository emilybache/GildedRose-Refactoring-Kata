using System;

namespace csharp.StrategyPatternExample
{
    class NormalDegradeStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            if (item.Quality > 0)
            {
                item.Quality--;
            }

            item.SellIn--;

            if (item.SellIn < 0 && item.Quality > 0)
            {
                item.Quality--;
            }            
        }
    }
}

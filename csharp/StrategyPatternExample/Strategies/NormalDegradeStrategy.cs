using System;

namespace csharp.StrategyPatternExample.Strategy
{
    /// <summary>
    /// Implements the strategy; SellIn and Quality values are lowered.
    /// Main strategy for the most items.
    /// </summary>
    internal class NormalDegradeStrategy : ICategoryStrategy
    {
        #region Methods
        
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

        #endregion
    }
}

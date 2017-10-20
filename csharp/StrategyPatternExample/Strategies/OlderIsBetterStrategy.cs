using System;

namespace csharp.StrategyPatternExample.Strategy
{
    /// <summary>
    /// Implements the strategy; Increase in Quality the older it gets.
    /// </summary>
    internal class OlderIsBetterStrategy : ICategoryStrategy
    {
        #region Methods
        
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

        #endregion
    }
}

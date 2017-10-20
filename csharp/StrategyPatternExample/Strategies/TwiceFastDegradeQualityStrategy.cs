using System;

namespace csharp.StrategyPatternExample.Strategy
{
    /// <summary>
    /// Implements the strategy; Items degrade in Quality twice as fast as normal items.
    /// </summary>
    internal class TwiceFastDegradeQualityStrategy : ICategoryStrategy
    {
        #region Methods
        
        public void Update(Item item)
        {
            int degrade = 2;

            item.SellIn--;

            if (item.SellIn < 0)
            {
                degrade = 4;
            }

            if (item.Quality > Global.MINIMUM_QUALITY)
            {
                item.Quality -= degrade;
            }

            if (item.Quality < Global.MINIMUM_QUALITY)
            {
                item.Quality = Global.MINIMUM_QUALITY;
            }
        }

        #endregion
    }
}

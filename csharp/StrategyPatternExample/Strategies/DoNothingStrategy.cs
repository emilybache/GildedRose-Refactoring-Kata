using System;

namespace csharp.StrategyPatternExample.Strategy
{
    /// <summary>
    /// Implements the strategy; Do nothing. No action is implemented.
    /// </summary>
    internal class DoNothingStrategy : ICategoryStrategy
    {
        #region Methods
        
        public void Update(Item item)
        {            
        }

        #endregion
    }
}

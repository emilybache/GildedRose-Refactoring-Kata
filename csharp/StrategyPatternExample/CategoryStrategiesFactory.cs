using csharp.StrategyPatternExample.Strategy;
using System;
using System.Collections.Generic;

namespace csharp.StrategyPatternExample
{
    /// <summary>
    /// This class is responsible for providing a category strategy to a given item.
    /// </summary>
    static internal class CategoryStrategiesFactory
    {
        #region Methods

        static public ICategoryStrategy GetCategoryStrategies(Item item)
        {
            switch (item.Name)
            {
                case Global.NAME_ITEM_AGED_BRIE:
                    return new OlderIsBetterStrategy();

                case Global.NAME_ITEM_BACKSTAGE_PASSES:
                    return new CloseExpiredImproveQualityStrategy(new List<CloseExpiredImproveQualityStrategy.NextExpiredCondition>() {
                        new CloseExpiredImproveQualityStrategy.NextExpiredCondition()
                        {
                            SellInLimit = 5,
                            Increment = 3
                        },
                        new CloseExpiredImproveQualityStrategy.NextExpiredCondition()
                        {
                            SellInLimit = 10,
                            Increment = 2
                        }
                    });

                case Global.NAME_ITEM_SULFURAS:
                    return new DoNothingStrategy();

                case Global.NAME_ITEM_CONJURED:
                    return new TwiceFastDegradeQualityStrategy();

                default:
                    return new NormalDegradeStrategy();
            }
        }
        
        #endregion
    }
}

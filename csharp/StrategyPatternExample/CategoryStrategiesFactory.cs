using csharp.StrategyPatternExample.Strategy;
using System;
using System.Collections.Generic;

namespace csharp.StrategyPatternExample
{
    class CategoryStrategiesFactory
    {
        #region Variables

        static private CategoryStrategiesFactory _strategiesFactory = null;

        #endregion

        #region Constructor

        private CategoryStrategiesFactory() { }

        #endregion

        #region Methods

        public static CategoryStrategiesFactory GetInstance()
        {
            if (_strategiesFactory == null)
            {
                _strategiesFactory = new CategoryStrategiesFactory();
            }

            return _strategiesFactory;
        }
        
        public List<ICategoryStrategy> GetCategoryStrategies(Item item)
        {
            List<ICategoryStrategy> listCategoryStrategies = new List<ICategoryStrategy>();

            if (item.Name == Global.NAME_ITEM_AGED_BRIE)
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new OlderIsBetterStrategy()
                };

            }
            else if (item.Name == Global.NAME_ITEM_BACKSTAGE_PASSES)
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new CloseExpiredImproveQualityStrategy(new List<CloseExpiredImproveQualityStrategy.NextExpiredCondition>() {
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
                    })
                };
            }
            else if (item.Name == Global.NAME_ITEM_SULFURAS)
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                };
            }
            else if (item.Name == Global.NAME_ITEM_CONJURED)
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new TwiceFastDegradeQualityStrategy()
                };
            }
            else
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new NormalDegradeStrategy()
                };
            }

            return listCategoryStrategies;
        }
        
        #endregion
    }
}

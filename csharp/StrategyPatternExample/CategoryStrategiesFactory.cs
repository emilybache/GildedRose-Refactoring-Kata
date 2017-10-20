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

            if (item.Name == "Aged Brie")
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new OlderIsBetterStrategy()
                };

            }
            else if (item.Name == "Backstage passes to a TAFKAL80ETC concert")
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                    new NextExpiredImproveQualityStrategy()
                };
            }
            else if (item.Name == "Sulfuras, Hand of Ragnaros")
            {
                listCategoryStrategies = new List<ICategoryStrategy>()
                {
                };
            }
            else if (item.Name == "Conjured Mana Cake")
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

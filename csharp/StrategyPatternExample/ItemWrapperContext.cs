using System;
using System.Collections.Generic;

namespace csharp.StrategyPatternExample
{
    /// <summary>
    /// Wrapper class to allow to create a context for every defined item and thus to be able to bind a strategy. 
    /// </summary>
    public class ItemWrapperContext
    {
        #region Variables
                        
        private Item _item;
        private ICategoryStrategy strategy;

        #endregion

        #region Properties

        public string Name
        {
            get { return this._item.Name; }
            set { this._item.Name = value; }
        }

        public int Quality
        {
            get { return this._item.Quality; }
            set { this._item.Quality = value; }
        }

        public int SellIn
        {
            get { return this._item.SellIn; }
            set { this._item.SellIn = value; }
        }

        #endregion

        #region Constructor

        public ItemWrapperContext(Item item)
        {
            this._item = item;
            this.strategy = CategoryStrategiesFactory.GetCategoryStrategies(item);
        }

        #endregion

        #region Methods

        public void UpdateQuality()
        {
            strategy.Update(this._item);
        }

        #endregion
    }
}

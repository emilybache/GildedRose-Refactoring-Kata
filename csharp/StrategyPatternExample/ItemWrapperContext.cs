using System;
using System.Collections.Generic;

namespace csharp.StrategyPatternExample
{
    public class ItemWrapperContext
    {
        #region Variables
                        
        private Item _item;
        private List<ICategoryStrategy> listCategoryStrategies;

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

            listCategoryStrategies = CategoryStrategiesFactory.GetInstance().GetCategoryStrategies(item);
        }

        #endregion

        #region Methods

        public void UpdateQuality()
        {
            foreach (ICategoryStrategy categoryStrategyItem in this.listCategoryStrategies)
            {
                categoryStrategyItem.Update(this._item);
            }
        }

        #endregion
    }
}

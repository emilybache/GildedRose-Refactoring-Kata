using System;

namespace csharp.Strategy
{
    public class ItemWrapperContext
    {
        #region Variables

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

        //private List<ICategoryStrategy> listCategoryStrategies;
        private Item _item;

        #endregion

        #region Constructor

        public ItemWrapperContext(Item item)
        {
            this._item = item;

            //listCategoryStrategies = StrategiesFactory.GetInstance().GetCategoryStrategies(item);
        }

        #endregion

        #region Methods

        //public void UpdateQuality()
        //{
        //    foreach (ICategoryStrategy categoryStrategyItem in this.listCategoryStrategies)
        //    {
        //        categoryStrategyItem.UpdateQuality(this);
        //    }
        //}

        #endregion

    }
}

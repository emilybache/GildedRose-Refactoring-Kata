using System.Linq;
using System.Collections.Generic;
using csharp.StrategyPatternExample;

namespace csharp.StrategyPatternExample
{
    /// <summary>
    /// GildedRose class refactored to adapt the strategy pattern without making a great refactor of main method of Program.cs.
    /// </summary>
    public class GildedRoseStrategyPatternExample : IGildedRoseApp
    {
        #region Variables
        
        private IList<ItemWrapperContext> Items;

        #endregion

        #region Constructor

        public GildedRoseStrategyPatternExample(IList<Item> Items)
        {            
            this.Items = Items.Select(i => new ItemWrapperContext(i)).ToList<ItemWrapperContext>();
        }

        #endregion

        #region Methods

        public void UpdateQuality()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                Items[i].UpdateQuality();
            }
        }

        #endregion
    }
}

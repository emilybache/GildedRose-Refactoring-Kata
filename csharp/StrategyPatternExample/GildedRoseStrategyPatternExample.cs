using System.Linq;
using System.Collections.Generic;
using csharp.StrategyPatternExample;

namespace csharp.StrategyPatternExample
{
    public class GildedRoseStrategyPatternExample : IGildedRoseApp
    {
        private IList<ItemWrapperContext> Items;

        public GildedRoseStrategyPatternExample(IList<Item> Items)
        {            
            this.Items = Items.Select(i => new ItemWrapperContext(i)).ToList<ItemWrapperContext>();
        }

        public void UpdateQuality()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                Items[i].UpdateQuality();
            }
        }
    }
}

using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRose
    {
        public GildedRose(List<Item> items)
        {
            Items = items;
        }

        public List<Item> Items { get; }

        public void UpdateQuality()
        {
            Items.ForEach(x => x.UpdateQuality());
        }
    }
}

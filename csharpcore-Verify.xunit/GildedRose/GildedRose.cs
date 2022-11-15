using System.Collections.Generic;

namespace GildedRoseKata
{
    public class GildedRose
    {
        private readonly IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }

        public void UpdateQuality()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                switch (Items[i])
                {
                    case LegendaryItem l: break;
                    case AgingItem a:
                        a.SetAgingItemQuality();
                        break;
                    case ConjuredItem c:
                        DecreaseItemProperties(c);
                        DecreaseItemQuality(c);
                        break;
                    default:
                        DecreaseItemProperties(Items[i]);
                        break;
                }
            }
        }

        private void DecreaseItemProperties(Item item)
        {
            DecreaseItemQuality(item);
            DecreateItemSellIn(item);
            DecreaseItemQuality(item);
        }

        private void DecreaseItemQuality(Item item)
        {
            if (item.Quality > 0)
            {
                item.Quality--;
            }
        }

        private void DecreateItemSellIn(Item item)
        {
            if (item is not LegendaryItem)
            {
                item.SellIn--;
            }
        }
        
    }
}

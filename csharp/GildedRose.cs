using System.Collections.Generic;

namespace csharp
{
    namespace csharp
    {
        public class GildedRose
        {
            IList<Item> Items;
            public GildedRose(IList<Item> Items)
            {
                this.Items = Items;
            }

            public void UpdateQuality()
            {
                foreach (var item in Items)
                {
                    UpdateItemQuality(item);
                    UpdateItemSellIn(item);
                }
            }

            private void UpdateItemQuality(Item item)
            {
                if (item.Name == "Sulfuras, Hand of Ragnaros")
                    return;

                if (item.Name == "Aged Brie")
                {
                    IncreaseQuality(item);
                    if (item.SellIn < 0)
                        IncreaseQuality(item);
                    return;
                }

                if (item.Name == "Backstage passes to a TAFKAL80ETC concert")
                {
                    IncreaseQuality(item);

                    if (item.SellIn <= 10)
                        IncreaseQuality(item);

                    if (item.SellIn <= 5)
                        IncreaseQuality(item);

                    if (item.SellIn < 0)
                        item.Quality = 0;
                    return;
                }

                DecreaseQuality(item);

                if (item.SellIn < 0)
                    DecreaseQuality(item);
            }

            private void UpdateItemSellIn(Item item)
            {
                if (item.Name != "Sulfuras, Hand of Ragnaros")
                    item.SellIn -= 1;
            }

            private void IncreaseQuality(Item item)
            {
                if (item.Quality < 50)
                    item.Quality += 1;
            }

            private void DecreaseQuality(Item item)
            {
                if (item.Quality > 0)
                    item.Quality -= 1;
            }
        }
    }

}

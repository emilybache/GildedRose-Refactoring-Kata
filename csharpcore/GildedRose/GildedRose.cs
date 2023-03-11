using System.Collections.Generic;

namespace GildedRoseKata
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
            for (var i = 0; i < Items.Count; i++)
            {
                if (Items[i].Name == "Sulfuras, Hand of Ragnaros")
                {
                    continue;
                }

                Items[i].SellIn = Items[i].SellIn - 1;

                if (Items[i].Name == "Aged Brie")
                {
                    IncreaseQualityByOne(i);
                    if (Items[i].SellIn < 0)
                    {
                        IncreaseQualityByOne(i);
                    }
                }
                else if (Items[i].Name == "Backstage passes to a TAFKAL80ETC concert")
                {
                    IncreaseBackstageQuality(i);
                    if (Items[i].SellIn < 0)
                    {
                        Items[i].Quality = 0;
                    }
                }
                else
                {
                    DecreaseQualityByOne(i);
                    if (Items[i].SellIn < 0)
                    {
                        DecreaseQualityByOne(i);
                    }
                }
            }
        }

        private void IncreaseBackstageQuality(int i)
        {
            IncreaseQualityByOne(i);

            if (Items[i].SellIn < 10)
            {
                IncreaseQualityByOne(i);
            }

            if (Items[i].SellIn < 5)
            {
                IncreaseQualityByOne(i);
            }
        }

        private void DecreaseQualityByOne(int i)
        {
            if (Items[i].Quality > 0)
            {
                Items[i].Quality = Items[i].Quality - 1;
            }
        }

        private void IncreaseQualityByOne(int i)
        {
            if (Items[i].Quality < 50)
            {
                Items[i].Quality = Items[i].Quality + 1;
            }
        }
    }
}

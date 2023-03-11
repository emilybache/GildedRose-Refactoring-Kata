using System;
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
                    SetQuality(i, quantity => quantity + 1, quantity  => quantity + 2);
                }
                else if (Items[i].Name == "Backstage passes to a TAFKAL80ETC concert")
                {
                    SetQuality(i, quantity => GetNewBackstageQuality(i), q => 0);
                }
                else
                {
                    SetQuality(i, quantity => quantity - 1, quantity => quantity - 2);
                }
            }
        }

        private void SetQuality(int itemIndex, Func<int, int> beforeSellBy, Func<int, int> afterSellBy)
        {
            bool wasAbove50 = Items[itemIndex].Quality > 50;

            if (Items[itemIndex].SellIn >= 0)
            {
                Items[itemIndex].Quality = beforeSellBy(Items[itemIndex].Quality);
            }
            else
            {
                Items[itemIndex].Quality = afterSellBy(Items[itemIndex].Quality);
            }

            if (!wasAbove50 && Items[itemIndex].Quality > 50)
            {
                Items[itemIndex].Quality = 50;
            }
            if (Items[itemIndex].Quality < 0)
            {
                Items[itemIndex].Quality = 0;
            }
        }

        private int GetNewBackstageQuality(int itemIndex)
        {
            int newQuantity = Items[itemIndex].Quality;
            ++newQuantity;

            if (Items[itemIndex].SellIn < 10)
            {
                ++newQuantity;
            }

            if (Items[itemIndex].SellIn < 5)
            {
                ++newQuantity;
            }
            return newQuantity;
        }
    }
}

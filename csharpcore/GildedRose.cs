using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRose
    {
        public const int MIN_QUALITY = 0;
        public const int MAX_QUALITY = 50;

        public const string SULFURA_ITEM = "sulfura";
        public const string AGED_BRIE_ITEM = "aged brie";
        public const string BACKSTAGE_PASS_ITEM = "backstage pass";
        public const string CONJURED_ITEM = "conjured";


        IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }

        public void UpdateQuality()
        {
            foreach (var item in Items)
            {
                var lowerCaseItemName = item.Name.ToLower();
                if (lowerCaseItemName.Contains(SULFURA_ITEM))
                {
                    HandleSulfuraItem(item);
                }
                else
                {
                    item.SellIn -= 1;
                    if (lowerCaseItemName.Contains(AGED_BRIE_ITEM))
                    {
                        HandleAgedBrieItem(item);
                    }
                    else if (lowerCaseItemName.Contains(BACKSTAGE_PASS_ITEM))
                    {
                        HandleBackstagePassItem(item);
                    }
                    else if (lowerCaseItemName.Contains(CONJURED_ITEM))
                    {
                        HandleConjuredItem(item);
                    }
                    else
                    {
                        HandleMiscelaniousItem(item);
                    }
                }
            }
        }

        private void HandleSulfuraItem(Item item)
        {
            // We do not alter anything on Sulfura items !
        }

        private void HandleAgedBrieItem(Item item)
        {
            if (item.SellIn > 0)
            {
                UpdateItemQualityValue(item, 1);
            }
            else
            {
                UpdateItemQualityValue(item, 2); // Is it correct ? This is to reflect old code but seems to not be in the spec
            }
        }

        private void HandleBackstagePassItem(Item item)
        {
            if (item.SellIn >= 10)
            {
                UpdateItemQualityValue(item, 1);
            }
            else if (item.SellIn >= 5)
            {
                UpdateItemQualityValue(item, 2);
            }
            else if (item.SellIn > 0)
            {
                UpdateItemQualityValue(item, 3);
            }
            else
            {
                item.Quality = MIN_QUALITY;
            }
        }

        private void HandleConjuredItem(Item item)
        {
            if (item.SellIn > 0)
            {
                UpdateItemQualityValue(item, -2);
            }
            else
            {
                UpdateItemQualityValue(item, -4);
            }
        }

        private void HandleMiscelaniousItem(Item item)
        {
            if (item.SellIn > 0)
            {
                UpdateItemQualityValue(item, -1);
            }
            else
            {
                UpdateItemQualityValue(item, -2);
            }
        }

        private void UpdateItemQualityValue(Item item, int qualityStep)
        {
            item.Quality += qualityStep;
            if (item.Quality < MIN_QUALITY)
            {
                item.Quality = MIN_QUALITY;
            }
            else if (item.Quality > MAX_QUALITY)
            {
                item.Quality = MAX_QUALITY;
            }
        }
    }
}

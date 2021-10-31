using System.Collections;
using System.Collections.Generic;

namespace csharp
{
    public class GildedRose
    {
        public const int sulfurasMaxQuality = 80;
        public const int itemMaxQuality = 50;
        public const int daysToDoublePassesQuantity = 11;
        public const int daysToTriplePassesQuantity = 6;
        public GildedRose()
        {
        }

        public void UpdateQuality(IList<Item> Items)
        {
            for (var i = 0; i < Items.Count; i++)
            {
                // Updating Quality of items

                if(Items[i].Name == "Aged Brie" && Items[i].Quality < itemMaxQuality)
                {
                    Items[i].Quality = Items[i].Quality + 1;
                }
                else if(Items[i].Name == "Backstage passes to a TAFKAL80ETC concert" && Items[i].Quality < itemMaxQuality)
                {

                    if (Items[i].SellIn < daysToTriplePassesQuantity)
                    {
                        Items[i].Quality = Items[i].Quality + 3;               
                    }

                    else if (Items[i].SellIn < daysToDoublePassesQuantity)
                    {
                        Items[i].Quality = Items[i].Quality + 2;
                    }
                    else
                    {
                        Items[i].Quality = Items[i].Quality +1;
                    }

                    if (Items[i].Quality > 50)
                    {
                        Items[i].Quality = 50;
                    }

                }
                else if(Items[i].Name == "Conjured Mana Cake" && Items[i].Quality > 0)
                {
                    Items[i].Quality = Items[i].Quality - 2;
                }
                else  if (Items[i].Quality > 0 && Items[i].Name != "Aged Brie" && Items[i].Name != "Backstage passes to a TAFKAL80ETC concert" && Items[i].Name != "Sulfuras, Hand of Ragnaros")
                {
                        Items[i].Quality = Items[i].Quality - 1;
                }


                // Selling Date
                if (Items[i].Name != "Sulfuras, Hand of Ragnaros")
                {
                    Items[i].SellIn = Items[i].SellIn - 1;
                }

                // Selling Date has passed
                if (Items[i].SellIn < 0)
                {
                   if(Items[i].Name == "Backstage passes to a TAFKAL80ETC concert")
                    {
                            Items[i].Quality = 0;
                    }  
                    else if(Items[i].Quality > 0 && Items[i].Name != "Sulfuras, Hand of Ragnaros" && Items[i].Name != "Aged Brie")
                    {
                        Items[i].Quality = Items[i].Quality - 1;
                    }
                }
            }
        }
    }
}

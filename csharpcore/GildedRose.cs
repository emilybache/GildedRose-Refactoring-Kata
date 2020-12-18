using System.Collections.Generic;

namespace csharpcore
{
    public class GildedRose
    {
        public static IList<Item> Items { get; set; }
        public GildedRose()
        {
        }

        public static void UpdateQuality()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                if (Items[i].Name != "Aged Brie" && Items[i].Name != "Backstage passes to a TAFKAL80ETC concert")
                {
                    if (Items[i].Quality > 0)
                    {
                        //Sulfuras is legedary item, never decrease in quality.
                        if (Items[i].Name != "Sulfuras, Hand of Ragnaros")
                        {
                            Items[i].Quality = Items[i].Quality - 1;

                            //this is for supplier item that will decrease twice
                            if (Items[i].Name == "Conjured Mana Cake")
                            {
                                Items[i].Quality = Items[i].Quality - 1;
                            }
                        }
                    }
                }
                else
                {
                    if (Items[i].Quality < 50)
                    {
                        //this line is for - "Aged Brie" the cheese actually increases in Quality the older it gets
                        Items[i].Quality = Items[i].Quality + 1;

                        if (Items[i].Name == "Backstage passes to a TAFKAL80ETC concert")
                        {
                            //Quality increases by 2 when there are 10 days or less 
                            if (Items[i].SellIn < 11)
                            {
                                if (Items[i].Quality < 50)
                                {
                                    Items[i].Quality = Items[i].Quality + 1;
                                }
                            }

                            //Quality increases by 3 when there are 5 days or less 
                            if (Items[i].SellIn < 6)
                            {
                                if (Items[i].Quality < 50)
                                {
                                    Items[i].Quality = Items[i].Quality + 1;
                                }
                            }
                        }
                    }
                }

                //Sulfuras is legedary item, never sold so don decrease the SellIn value.
                if (Items[i].Name != "Sulfuras, Hand of Ragnaros")
                {
                    Items[i].SellIn = Items[i].SellIn - 1;
                }

                //When date has passed it means SeeIn < = 0 then Quality degrades twice as fast
                if (Items[i].SellIn < 0)
                {
                    if (Items[i].Name != "Aged Brie")
                    {
                        if (Items[i].Name != "Backstage passes to a TAFKAL80ETC concert")
                        {
                            if (Items[i].Quality > 0)
                            {
                                if (Items[i].Name != "Sulfuras, Hand of Ragnaros")
                                {
                                    Items[i].Quality = Items[i].Quality - 1;

                                    //this is for supplier item that will decrease twice
                                    if (Items[i].Name == "Conjured Mana Cake")
                                    {
                                        Items[i].Quality = Items[i].Quality - 1;
                                    }
                                }
                            }
                        }
                        else
                        {
                            //Quality drops to 0 after the concert for "Backstage passes"
                            Items[i].Quality = Items[i].Quality - Items[i].Quality;
                        }
                    }
                    else
                    {
                        if (Items[i].Quality < 50)
                        {
                            Items[i].Quality = Items[i].Quality + 1;
                        }
                    }
                }
            }
        }
    }
}

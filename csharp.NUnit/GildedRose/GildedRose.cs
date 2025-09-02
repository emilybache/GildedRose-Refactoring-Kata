using System.Collections.Generic;

namespace GildedRoseKata;

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
            if (item.Name == "Sulfuras, Hand of Ragnaros")
                continue;

            item.SellIn--;

            switch (item.Name)
            {
                case "Aged Brie":
                    IncreaseQuality(item);
                    if (item.SellIn < 0)
                        IncreaseQuality(item);
                    break;

                case "Backstage passes to a TAFKAL80ETC concert":
                    if (item.SellIn < 0)
                    {
                        item.Quality = 0;
                    }
                    else
                    {
                        IncreaseQuality(item);
                        if (item.SellIn < 10) IncreaseQuality(item);
                        if (item.SellIn < 5) IncreaseQuality(item);
                    }
                    break;

                default:
                    DecreaseQuality(item);
                    if (item.SellIn < 0)
                        DecreaseQuality(item);
                    break;
            }
        }
    }

    private void IncreaseQuality(Item item)
    {
        if (item.Quality < 50) item.Quality++;
    }

    private void DecreaseQuality(Item item)
    {
        if (item.Quality > 0) item.Quality--;
    }
}
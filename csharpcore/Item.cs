using System;

namespace csharpcore
{
    public class Item
    {
        public string Name { get; set; }
        public int SellIn { get; set; }
        public int Quality { get; set; }

        public virtual void UpdateQuality()
        {
            Quality -= 1;

            SellIn -= 1;

            if (SellIn < 0)
            {
                Quality -= 1;
            }

            if (Quality < 0)
            {
                Quality = 0;
            }
        }
    }

    public class AgedBrieItem : Item
    {
        public AgedBrieItem()
        {
            Name = "Aged Brie";
        }

        public override void UpdateQuality()
        {
            Quality += 1;

            SellIn -= 1;

            if (SellIn < 0)
            {
                Quality += 1;
            }

            if (Quality > 50)
            {
                Quality = 50;
            }
        }
    }

    public class BackstagePassesItem : Item
    {
        public BackstagePassesItem()
        {
            Name = "Backstage passes to a TAFKAL80ETC concert";
        }

        public override void UpdateQuality()
        {
            Quality += 1;

            if (SellIn < 11)
            {
                Quality += 1;
            }

            if (SellIn < 6)
            {
                Quality += 1;
            }

            if (Quality > 50)
            {
                Quality = 50;
            }

            SellIn -= 1;

            if (SellIn < 0)
            {
                Quality = 0;
            }
        }
    }

    public class SulfurasItem : Item
    {
        public SulfurasItem()
        {
            Name = "Sulfuras, Hand of Ragnaros";
        }

        public override void UpdateQuality() { }
    }

    public class ConjuredItem : Item
    {
        public override void UpdateQuality()
        {
            Quality -= 2;

            SellIn -= 1;

            if (SellIn < 0)
            {
                Quality -= 2;
            }

            if (Quality < 0)
            {
                Quality = 0;
            }
        }
    }
}

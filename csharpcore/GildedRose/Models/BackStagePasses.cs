using GildedRose.Abstraction;
using GildedRoseKata;

namespace GildedRose.Models
{
    public class BackStagePasses : Item, ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public void UpdateQuality()
        {
            if (this.SellDaysGone > this.SellIn)
            {
                this.Quality = 0;
            }

            if ((this.SellIn - this.SellDaysGone) <= 5)
            {
                if (this.Quality < 51)
                    this.Quality += 3;
            }

            if ((this.SellIn - this.SellDaysGone) <= 10)
            {
                if (this.Quality < 51)
                    this.Quality += 2;
            }
        }

        public void UpdateSellIn()
        {
            if (this.SellIn > 0)
                this.SellIn--;
        }
    }
}
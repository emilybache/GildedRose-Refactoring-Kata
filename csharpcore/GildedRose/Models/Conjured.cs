using GildedRose.Abstraction;
using GildedRoseKata;

namespace GildedRose.Models
{
    public class Conjured : Item, ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public void UpdateQuality()
        {
            if (this.SellDaysGone > this.SellIn && this.Quality > 1)
                this.Quality -= 2;
        }

        public void UpdateSellIn()
        {
            if (this.SellIn > 0)
                this.SellIn--;
        }
    }
}
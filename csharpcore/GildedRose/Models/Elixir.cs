using GildedRose.Abstraction;
using GildedRoseKata;

namespace GildedRose.Models
{
    public class Elixir : Item, ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public void UpdateQuality()
        {
            if (this.SellDaysGone > this.SellIn && this.Quality > 0)
                this.Quality --;
        }

        public void UpdateSellIn()
        {
            if (this.SellIn > 0)
                this.SellIn--;
        }
    }
}

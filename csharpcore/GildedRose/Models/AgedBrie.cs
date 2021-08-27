using GildedRose.Abstraction;
using GildedRoseKata;

namespace GildedRose.Models
{
    public class AgedBrie : Item, ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public void UpdateQuality() {
            if (this.Quality < 51)
            {
                this.Quality++;
            }
        }

        public void UpdateSellIn()
        {
            if (this.SellIn > 0)
                this.SellIn--;
        }
    }
}
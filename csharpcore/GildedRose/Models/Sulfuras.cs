using GildedRose.Abstraction;
using GildedRoseKata;

namespace GildedRose.Models
{
    public class Sulfuras : Item, ICustomMethod
    {
        public int SellDaysGone { get; set; }

        public void UpdateQuality()
        {
            // do thing
        }

        public void UpdateSellIn()
        {
            // do nothing
        }
    }
}

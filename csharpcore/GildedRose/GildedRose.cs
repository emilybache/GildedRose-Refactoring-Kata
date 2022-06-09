using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;

namespace GildedRoseKata
{
    public class GildedRose
    {
        IList<Item> Items;
        public GildedRose(IList<Item> Items)
        {
            this.Items = Items;
        }

        public void UpdateQuality()
        {
            var itemList = Items.Where(c => c.Name.Equals("Aged Brie")).ToList(); // "Aged Brie" actually increases in Quality the older it gets
            foreach (var item in itemList)
            {
                item.Quality = SetQuality(item.Quality, true);
                item.ProcessedOn = DateTime.Now;
            }
            itemList = Items.Where(c => c.Name.Contains("Sulfuras")).ToList(); // "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
            foreach (var item in itemList)
            {
                item.ProcessedOn = DateTime.Now;
            }
            itemList = Items.Where(c => c.Name.Contains("Backstage pass")).ToList(); // "Backstage passes", like aged brie, increases in Quality as it's SellIn value approaches; Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but Quality drops to 0 after the concert... need requirements clarification. If backstage pass is more than 10 days increase or decrease? Process as normal for now
            foreach (var item in itemList)
            {
                var daysToConcert = (item.SellBy - DateTime.Now).TotalDays;
                if (daysToConcert <= 0)
                {
                    item.Quality = 0;
                    item.ProcessedOn = DateTime.Now;
                    continue;
                }
                if (daysToConcert <= 5)
                {
                    item.Quality = SetQuality(item.Quality, true, 3);
                    item.ProcessedOn = DateTime.Now;
                    continue;
                }
                if (daysToConcert <= 10)
                {
                    item.Quality = SetQuality(item.Quality, true, 2);
                    item.ProcessedOn = DateTime.Now;
                    continue;
                }
                item.Quality = SetQuality(item.Quality);
                item.ProcessedOn = DateTime.Now;
            }
            DateTime yesterday = DateTime.Today.AddDays(-1);
            itemList = Items.Where(t => t.ProcessedOn <= yesterday).ToList(); // Get everything else that hasn't been processed today
            foreach (var item in itemList)
            {
                item.Quality = SetQuality(item.Quality);
                item.ProcessedOn = DateTime.Now;
            }
        }

        private int SetQuality(int qual, bool increase = false, int value = 1)
        {
            if (increase)
            {
                qual = qual + value;
                if (qual > 50) { qual = 50; }
            }
            else
            {
                qual = qual - value;
                if (qual < 0) { qual = 0; }
            }
            return qual;
        }
    }
}

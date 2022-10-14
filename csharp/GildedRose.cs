using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.WindowsRuntime;

namespace csharp
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
            foreach (var item in Items)
            {
                if (item.Name.ToLower().Contains("backstage passes")) { BackstagePassesDailyChange(item); continue; }
                else if (item.Name.ToLower().Contains("aged")) { AgingItemDailyChange(item); continue; }
                else if (item.Name.ToLower().Contains("conjured")) { ConjuredItemDailyChange(item); continue; }
                else if (!item.Name.ToLower().Contains("sulfuras")) NormalItemsDailyChange(item);

            }
        }

        //Conjured Items quality drops twice faster than normal items quality
        private void ConjuredItemDailyChange(Item conjuredItem)
        {
            conjuredItem.SellIn--;
            if (conjuredItem.SellIn < 0)
            {
                if (conjuredItem.Quality == 0) return;
                else if (conjuredItem.Quality < 4) { conjuredItem.Quality = 0; return; }
                else conjuredItem.Quality -= 4;
                return;
            }
            else
            {
                if (conjuredItem.Quality == 0) return;
                else if (conjuredItem.Quality < 2) { conjuredItem.Quality = 0; return; }
                else conjuredItem.Quality -= 2;
            }
        }
        //Items quality drops by quality rate each time day ends and if  sellin < 0 drop rate is dbled
        private void NormalItemsDailyChange(Item normalItem)
        {
            normalItem.SellIn--;
            if (normalItem.SellIn < 0)
            {
                if (normalItem.Quality == 0) return;
                else if (normalItem.Quality < 2) { normalItem.Quality = 0; return; }
                else normalItem.Quality -= 2;
                return;
            }
            else
            {
                if (normalItem.Quality == 0) return;
                else if (normalItem.Quality < 1) { normalItem.Quality = 0; return; }
                else normalItem.Quality--;
            }
        }

        private void BackstagePassesDailyChange(Item pass)
        {
            pass.SellIn--;
            if (pass.SellIn < 0)
            {
                if (pass.Quality == 0) return;
                else pass.Quality = 0; return;
            } 
            else if (pass.SellIn <= 5)
            {
                if (pass.Quality == 50) return;
                else if (pass.Quality >= 47) { pass.Quality = 50; return; }
                else pass.Quality += 3;
                return;
            }
            else if (pass.SellIn <= 10)
            {
                if (pass.Quality == 50) return;
                else if (pass.Quality >= 48) { pass.Quality = 50; return; }
                else pass.Quality += 2;
                return;
            }
            else
            {
                if (pass.Quality == 50) return;
                else pass.Quality++;
            }
        }

        private void AgingItemDailyChange(Item agingItem)
        {
            agingItem.SellIn--;
            if (agingItem.SellIn < 0)
            {
                if (agingItem.Quality == 50) return;
                else if (agingItem.Quality >= 49) { agingItem.Quality = 50; return; }
                else agingItem.Quality += 2;
                return;
            }
            else
            {
                if (agingItem.Quality == 50) return;
                else agingItem.Quality++;
            }
        }
    }
}

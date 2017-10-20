using System;

namespace csharp.StrategyPatternExample.Strategy
{
    class NextExpiredImproveQualityStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            item.SellIn--;

            if (item.SellIn < 0)
            {
                item.Quality = Global.MINIMUN_QUALITY;
            }
            else
            {
                int inc = 1;

                if (item.SellIn < 5)
                {
                    inc = 3;
                }
                else if (item.SellIn < 10)
                {
                    inc = 2;
                }

                item.Quality += inc;

                if (item.Quality > Global.MAXIMUN_QUALITY)
                {
                    item.Quality = Global.MAXIMUN_QUALITY;
                }
            }
        }
    }
}

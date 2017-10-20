using System;

namespace csharp.Strategy
{
    class NextExpiredImproveQualityStrategy : ICategoryStrategy
    {
        public void Update(Item item)
        {
            item.SellIn--;

            if (item.SellIn < 0)
            {
                item.Quality = 0;
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

                if (item.Quality > 50)
                {
                    item.Quality = 50;
                }
            }
        }
    }
}

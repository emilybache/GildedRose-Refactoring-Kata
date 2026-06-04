package com.gildedrose.strategy;

import com.gildedrose.Item;

public class AgedBrieStrategy  implements ItemStrategyUpdate{
    @Override
    public void update(Item item)
    {
        item.sellIn--;
        if (item.quality < 50)
            item.quality++;
        if(item.quality < 50 && item.sellIn < 0)
            item.quality++;
    }
    
}

package com.gildedrose.strategy;

import com.gildedrose.Item;

public class ConcertStrategy implements ItemStrategyUpdate{
    @Override
    public void update(Item item){
        if (item.sellIn == 0)
            item.quality = 0;
        else {
            if (item.quality < 50) 
                item.quality++; 
            if (item.sellIn < 11 && item.quality < 50) 
                item.quality++;
            if (item.sellIn < 6 && item.quality < 50) 
                item.quality++;
        }
        item.sellIn--;

    }
}

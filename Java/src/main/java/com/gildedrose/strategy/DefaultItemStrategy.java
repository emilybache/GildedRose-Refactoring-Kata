package com.gildedrose.strategy;
 
import com.gildedrose.Item;

public class DefaultItemStrategy implements ItemStrategyUpdate{
    @Override
    public void update(Item item){
        if (item.quality > 0)
            item.quality--;
        item.sellIn--;
        if(item.sellIn < 0 && item.quality > 0)
            item.quality--;
    }
    
}

package com.gildedrose.business;

import com.gildedrose.Item;

import java.util.Arrays;
import java.util.List;

public class RegularItemHandler implements ItemHandler {

    private static final List<String> LEGENDARY_ITEMS =  Arrays.asList(ItemEnum.SULFURAS.getValue());

    @Override
    public void updateItem(Item item) {
        if (!LEGENDARY_ITEMS.contains(item.name)) {
            decrementQuality(item, item.quality > 0);
            decrementSellIn(item);
            decrementQuality(item,item.sellIn < 0 && item.quality > 0);
        }
    }
}


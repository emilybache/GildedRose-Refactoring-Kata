package com.gildedrose.business;

import com.gildedrose.Item;

import java.util.Arrays;
import java.util.List;

public class RegularItemHandler implements ItemHandler {

    private static final List<String> LEGENDARY_ITEMS =  Arrays.asList(new String[]{ItemEnum.SULFURAS.getValue()});

    @Override
    public void updateItem(Item item) {
        if (!LEGENDARY_ITEMS.contains(item.name)) {
            if (item.quality > 0) {
                decrementQuality(item);
            }
            decrementSellIn(item);

            if (hasExpired(item) && item.quality > 0) {
                decrementQuality(item);
            }
        }
    }
}


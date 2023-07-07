package com.gildedrose.business;

import com.gildedrose.Item;

import java.util.stream.IntStream;

public class ConjuredItemHandler implements ItemHandler {

    @Override
    public void updateItem(Item item) {
        IntStream.range(0, 2).forEach(i -> decrementQuality(item,item.quality > 0));
        decrementSellIn(item);
        IntStream.range(0, 2).forEach(i -> decrementQuality(item,item.sellIn < 0 && item.quality > 0));
    }
}

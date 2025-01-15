package com.gildedrose;

public class ConjuredItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        item.quality = item.quality > 2 ? item.quality - 2 : 0;
        item.sellIn = item.sellIn - 1;
    }
}

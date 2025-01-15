package com.gildedrose;

public class AgedBrieItemStrategyImpl implements ItemStrategy {
    @Override
    public void updateQuality(Item item) {
        if (item.quality < 50) {
            item.quality = item.quality + 1;
        }
        item.sellIn = item.sellIn - 1;
        if (item.sellIn < 0) {
            item.quality = item.quality < 50 ? item.quality + 1 : 50;
        }
    }
}

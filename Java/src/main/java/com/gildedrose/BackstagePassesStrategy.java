package com.gildedrose;

public enum BackstagePassesStrategy implements QualityUpdateStrategy {
    INSTANCE;

    @Override
    public void applyTo(Item item) {
        if (item.sellIn <= 0) {
            item.quality = 0;
        } else if (item.sellIn >= 10) {
            item.quality = Math.min(50, item.quality+1);
        } else if (item.sellIn >= 5) {
            item.quality = Math.min(50, item.quality+2);
        } else {
            item.quality = Math.min(50, item.quality+3);
        }
        --item.sellIn;
    }
}

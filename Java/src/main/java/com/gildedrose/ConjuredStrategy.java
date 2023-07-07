package com.gildedrose;

public enum ConjuredStrategy implements QualityUpdateStrategy {
    INSTANCE;

    @Override
    public void applyTo(Item item) {
        if (item.sellIn > 0) {
            item.quality = Math.max(0, item.quality-2);
        } else {
            item.quality = Math.max(0, item.quality-4);
        }
        --item.sellIn;
    }
}

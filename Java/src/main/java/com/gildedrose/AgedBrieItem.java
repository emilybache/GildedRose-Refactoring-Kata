package com.gildedrose;

class AgedBrieItem extends UpdatableItem {
    public AgedBrieItem(Item item) {
        super(item);
    }

    @Override
    public void update() {
        incrementQuality(STANDARD_VALUE_CHANGE);
        decrementSellIn();
    }
}

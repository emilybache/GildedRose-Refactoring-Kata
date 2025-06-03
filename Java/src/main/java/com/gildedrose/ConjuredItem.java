package com.gildedrose;

class ConjuredItem extends UpdatableItem {

    public ConjuredItem(Item item) {
        super(item);
    }

    @Override
    public void update() {
        decrementQuality(DOUBLE_VALUE_CHANGE);
        decrementSellIn();
    }
}

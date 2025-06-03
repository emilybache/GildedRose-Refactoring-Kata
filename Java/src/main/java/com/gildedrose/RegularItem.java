package com.gildedrose;

class RegularItem extends UpdatableItem {
    public RegularItem(Item item) {
        super(item);
    }

    @Override
    public void update() {
        decrementSellIn();
        updateQuality();
    }

    private void updateQuality() {
        int decrementAmount = isOutdated() ? DOUBLE_VALUE_CHANGE : STANDARD_VALUE_CHANGE;
        decrementQuality(decrementAmount);
    }
}


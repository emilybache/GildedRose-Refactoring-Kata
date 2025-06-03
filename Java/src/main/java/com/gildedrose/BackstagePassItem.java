package com.gildedrose;

class BackstagePassItem extends UpdatableItem {
    private static final int FIRST_SELL_IN_THRESHOLD = 10;
    private static final int SECOND_SELL_IN_THRESHOLD = 5;

    public BackstagePassItem(Item item) {
        super(item);
    }

    @Override
    public void update() {
        decrementSellIn();
        updateQuality();
    }

    private void updateQuality() {
        if (isOutdated()) {
            setZeroQuality();
        } else {
            incrementQuality(getIncrementValue());
        }
    }

    private int getIncrementValue() {
        int increment = STANDARD_VALUE_CHANGE;
        if (item.sellIn <= FIRST_SELL_IN_THRESHOLD) increment++;
        if (item.sellIn <= SECOND_SELL_IN_THRESHOLD) increment++;
        return increment;
    }
}


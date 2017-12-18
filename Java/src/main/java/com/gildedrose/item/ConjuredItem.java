package com.gildedrose.item;

public class ConjuredItem implements CustomisedItem {

    private final Item item;

    public ConjuredItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        decreaseSellByDayValueByOne();
        if (sellByDayValueIsOverZero()) {
            decreaseQualityBy(2);
        } else {
            decreaseQualityBy(4);
        }
    }

    private void decreaseSellByDayValueByOne() {
        item.sellIn -= 1;
    }

    private boolean sellByDayValueIsOverZero() {
        return item.sellIn > 0;
    }

    private void decreaseQualityBy(int qualityValue) {
        item.quality -= qualityValue;
    }
}

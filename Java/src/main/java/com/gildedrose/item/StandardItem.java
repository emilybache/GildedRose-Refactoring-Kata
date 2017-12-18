package com.gildedrose.item;

public class StandardItem implements CustomisedItem {

    private final Item item;

    public StandardItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        decreaseSellByDayValueByOne();
        if (sellByDayValueIsOverZero()) {
            decreaseQualityBy(1);
        } else {
            decreaseQualityBy(2);
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

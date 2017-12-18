package com.gildedrose.item;

public class AgedBrie implements CustomisedItem {

    private final Item item;

    public AgedBrie(Item item) {
        this.item = item;
    }

    public void updateState() {
        decreaseSellByDayValueByOne();
        increaseQualityByOne();
    }

    private void decreaseSellByDayValueByOne() {
        item.sellIn -= 1;
    }

    private void increaseQualityByOne() {
        item.quality += 1;
    }
}

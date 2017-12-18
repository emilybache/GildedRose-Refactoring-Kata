package com.gildedrose.item;

public class StandardItem implements CustomisedItem {

    private final Item item;

    public StandardItem(Item item) {
        this.item = item;
    }

    public void updateState() {
        decreaseSellByDayValueByOne();
        if (sellByDayValueIsOverZero()) {
            decreaseQualityBy(decreasingValueOverZeroDaysToSell());
        } else {
            decreaseQualityBy(decreasingValueForZeroOrLessDaysToSell());
        }
    }

    public int decreasingValueOverZeroDaysToSell() {
        return 1;
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

    private int decreasingValueForZeroOrLessDaysToSell() {
        return decreasingValueOverZeroDaysToSell() * 2;
    }
}

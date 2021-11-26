package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;

import static com.gildedrose.items.LegendaryItem.NOT_LEGENDARY_ITEM_ERROR_MESSAGE;
import static com.gildedrose.items.LegendaryItem.isNotLegendary;
import static java.lang.Math.max;

public class ItemHandler {

    public static final String QUALITY_ERROR_MESSAGE = "Quality cannot be negative! Current value: ";

    private final Item item;

    public ItemHandler(Item item) {
        this.item = item;
    }

    static void validate(Item item) {
        if (qualityIsNegative(item)) {
            throw new IllegalArgumentException(QUALITY_ERROR_MESSAGE + item.quality);
        }
        if (isNotLegendary(item)) {
            throw new IllegalArgumentException(NOT_LEGENDARY_ITEM_ERROR_MESSAGE + item.quality);
        }
    }

    private static boolean qualityIsNegative(Item item) {
        return item.quality < 0;
    }

    public boolean qualityIsHigherThanZero() {
        return item.quality > 0;
    }

    public void makeQualityZero() {
        item.quality = 0;
    }

    public void decrementSellInDate() {
        item.sellIn--;
    }

    public boolean beforeSellInDate() {
        return item.sellIn >= 0;
    }

    public boolean sellInLessThan5Days() {
        return item.sellIn >= 0 && item.sellIn <= 5;
    }

    public boolean sellInLessThan10Days() {
        return item.sellIn >= 5 && item.sellIn <= 10;
    }

    public boolean sellInMoreThan10Days() {
        return item.sellIn >= 10;
    }

    public void incrementQuality() {
        item.quality++;
    }

    public void incrementQualityBy2() {
        item.quality = max(item.quality + 2, 0);
    }

    public void incrementQualityBy3() {
        item.quality = max(item.quality + 3, 0);
    }

    public void decrementQuality() {
        item.quality--;
    }

    public void decrementQualityBy2() {
        item.quality = max(item.quality - 2, 0);
    }

    public void decrementQualityBy4() {
        item.quality = max(item.quality - 4, 0);
    }

}

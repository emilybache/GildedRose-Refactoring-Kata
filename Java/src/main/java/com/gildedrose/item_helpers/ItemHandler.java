package com.gildedrose.item_helpers;

import com.gildedrose.Item;

import static com.gildedrose.item_helpers.ItemName.LEGENDARY;
import static java.lang.Math.max;

public class ItemHandler {

    private static final int LEGENDARY_ITEM_QUALITY = 80;
    protected static final String NEGATIVE_QUALITY_ERROR_MESSAGE = "Quality cannot be negative! Current value: ";
    protected static final String NOT_LEGENDARY_ITEM_ERROR_MESSAGE = "Item is legendary, quality must be always 80! Current value: ";

    private final Item item;

    public ItemHandler(Item item) {
        this.item = item;
    }

    static void validate(Item item) {
        if (qualityIsNegative(item)) {
            throw new IllegalArgumentException(NEGATIVE_QUALITY_ERROR_MESSAGE + item.quality);
        }
        if (isNotLegendary(item)) {
            throw new IllegalArgumentException(NOT_LEGENDARY_ITEM_ERROR_MESSAGE + item.quality);
        }
    }

    private static boolean qualityIsNegative(Item item) {
        return item.quality < 0;
    }

    private static boolean isNotLegendary(Item item) {
        return item.name.equals(LEGENDARY.toString()) && item.quality != 80;
    }

    public void decrementSellInDate() {
        item.sellIn--;
    }

    public boolean beforeSellInDate() {
        return item.sellIn >= 0;
    }

    public void incrementQualityByTwo() {
        item.quality = max(item.quality + 2, 0);
    }

    public void incrementQuality() {
        item.quality++;
    }

    public void decrementQuality() {
        item.quality--;
    }

    public void decrementQualityBy4() {
        item.quality = max(item.quality - 4, 0);
    }

    public void decrementQualityBy2() {
        item.quality = max(item.quality - 2, 0);
    }

    public void setLegendaryQuality() {
        if (item.quality != LEGENDARY_ITEM_QUALITY) {
            item.quality = LEGENDARY_ITEM_QUALITY;
        }
    }

    public boolean sellInLessThan5Days() {
        return item.sellIn >= 0 && item.sellIn <= 5;
    }

    public boolean sellInLessThan10Days() {
        return item.sellIn >= 5 && item.sellIn <= 10;
    }

    public boolean sellInDaysMoreThan10Days() {
        return item.sellIn >= 10;
    }

    public boolean qualityIsHigherThanZero() {
        return item.quality > 0;
    }

    public void makeQualityZero() {
        item.quality = 0;
    }

    public void incrementQualityBy3() {
        item.quality = max(item.quality + 3, 0);
    }

}

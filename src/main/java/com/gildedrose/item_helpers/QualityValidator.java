package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;

import static com.gildedrose.items.LegendaryItem.*;

public class QualityValidator {

    private QualityValidator() {
    }

    public static final String QUALITY_ERROR_MESSAGE = "Quality cannot be negative! Current value: ";
    public static final String OUT_OF_BOUND_QUALITY_MESSAGE = "Quality cannot be above 50! Current value: ";
    public static final String NOT_LEGENDARY_ITEM_ERROR_MESSAGE = "Item is legendary, quality must be always 80! Current value: ";

    public static void validateQuality(Item item) {
        if (isLegendaryWrongQuality(item)) {
            throw new IllegalArgumentException(NOT_LEGENDARY_ITEM_ERROR_MESSAGE + item.quality);
        } else if (qualityIsNegative(item)) {
            throw new IllegalArgumentException(QUALITY_ERROR_MESSAGE + item.quality);
        } else if (qualityIsAbove50(item)) {
            throw new IllegalArgumentException(OUT_OF_BOUND_QUALITY_MESSAGE + item.quality);
        }
    }

    public static boolean isLegendaryWrongQuality(Item item) {
        return isLegendary(item) && item.quality != LEGENDARY_ITEM_QUALITY;
    }

    private static boolean qualityIsNegative(Item item) {
        return item.quality < 0;
    }

    private static boolean qualityIsAbove50(Item item) {
        return item.quality > 50 && isNotLegendary(item);
    }
}

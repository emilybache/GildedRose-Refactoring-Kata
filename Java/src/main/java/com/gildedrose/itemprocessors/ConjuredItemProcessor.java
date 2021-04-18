package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton;
import com.gildedrose.itemsorts.ConjuredItem;


public class ConjuredItemProcessor extends QualityItemProcessorSkeleton<ConjuredItem> implements QualityItemProcessor<ConjuredItem> {

    public static final int CONJURED_ITEM_QUALITY_CHANGE_MULTIPLIER = 2;

    @Override
    int applyQualityChangeMultiplier(int qualityChange) {
        return CONJURED_ITEM_QUALITY_CHANGE_MULTIPLIER * qualityChange;
    }
}

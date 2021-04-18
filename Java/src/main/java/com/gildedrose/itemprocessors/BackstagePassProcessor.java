package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton;
import com.gildedrose.itemsorts.BackstagePass;

public class BackstagePassProcessor extends QualityItemProcessorSkeleton<BackstagePass> implements QualityItemProcessor<BackstagePass>{

    public static final int FIVE_DAYS_BEFORE_CONCERT = 5;
    public static final int TEN_DAYS_BEFORE_CONCERT = 10;

    @Override
    int qualityFunction(int oldQuality, int qualityChange) {
        return oldQuality + qualityChange;
    }

    @Override
    int calculateQualityChange(int sellIn, int oldQuality) {
        if (sellIn < 0) {
            return -oldQuality;
        } else if (sellIn < FIVE_DAYS_BEFORE_CONCERT) {
            return 3;
        } else if (sellIn < TEN_DAYS_BEFORE_CONCERT) {
            return 2;
        } else {
            return 1;
        }
    }
}

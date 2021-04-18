package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton;

import com.gildedrose.itemsorts.AgedBrie;

public class AgedBrieProcessor extends QualityItemProcessorSkeleton<AgedBrie> implements QualityItemProcessor<AgedBrie> {
    @Override
    int qualityFunction(int oldQuality, int qualityChange) {
        return oldQuality + qualityChange;
    }
}

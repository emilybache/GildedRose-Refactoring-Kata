package com.gildedrose.itemtorequest;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingRequest;
import com.gildedrose.itemsorts.QualityItem;

public class ProcessingRequestFromQualityItem {
    public <T extends QualityItem> ProcessingRequest<T> get(T qualityItem) {
        return new ProcessingRequest<>(qualityItem.getSellIn(), qualityItem.getQuality());
    }
}

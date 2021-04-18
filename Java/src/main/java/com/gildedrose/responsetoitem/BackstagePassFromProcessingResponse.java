package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.BackstagePass;

public class BackstagePassFromProcessingResponse implements QualityItemFromProcessingResponse<BackstagePass> {
    @Override
    public BackstagePass toQualityItem(BackstagePass qualityItemOld, ProcessingResponse<BackstagePass> processingResponse) {
        return new BackstagePass(qualityItemOld.getName(), processingResponse.getSellIn(), processingResponse.getQuality());
    }
}

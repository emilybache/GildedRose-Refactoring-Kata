package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.AgedBrie;

public class AgedBrieFromProcessingResponse implements QualityItemFromProcessingResponse<AgedBrie> {
    @Override
    public AgedBrie toQualityItem(AgedBrie qualityItemOld, ProcessingResponse<AgedBrie> processingResponse) {
        return new AgedBrie(qualityItemOld.getName(), processingResponse.getSellIn(), processingResponse.getQuality());
    }
}

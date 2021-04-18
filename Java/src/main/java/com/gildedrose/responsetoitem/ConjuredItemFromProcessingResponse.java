package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.ConjuredItem;

public class ConjuredItemFromProcessingResponse implements QualityItemFromProcessingResponse<ConjuredItem> {
    @Override
    public ConjuredItem toQualityItem(ConjuredItem qualityItemOld, ProcessingResponse<ConjuredItem> processingResponse) {
        return new ConjuredItem(qualityItemOld.getName(), processingResponse.getSellIn(), processingResponse.getQuality());
    }
}

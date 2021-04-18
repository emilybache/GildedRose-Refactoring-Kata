package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.NormalItem;


public class NormalItemFromProcessingResponse implements QualityItemFromProcessingResponse<NormalItem> {
    @Override
    public NormalItem toQualityItem(NormalItem qualityItemOld, ProcessingResponse<NormalItem> processingResponse) {
        return new NormalItem(qualityItemOld.getName(), processingResponse.getSellIn(), processingResponse.getQuality());
    }
}

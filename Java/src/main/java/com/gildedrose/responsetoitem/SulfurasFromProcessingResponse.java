package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.Sulfuras;

public class SulfurasFromProcessingResponse implements QualityItemFromProcessingResponse<Sulfuras> {
    @Override
    public Sulfuras toQualityItem(Sulfuras qualityItemOld, ProcessingResponse<Sulfuras> processingResponse) {
        return new Sulfuras(qualityItemOld.getName(), processingResponse.getSellIn(), processingResponse.getQuality());
    }
}

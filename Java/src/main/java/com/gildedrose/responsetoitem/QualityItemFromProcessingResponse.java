package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.QualityItem;

public interface QualityItemFromProcessingResponse<T extends QualityItem> {
    T toQualityItem(T qualityItemOld, ProcessingResponse<T> processingResponse);
}

package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.AgedBrie;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgedBrieFromProcessingResponseTest {

    @Test
    void toQualityItem() {
        AgedBrieFromProcessingResponse agedBrieFromProcessingResponse = new AgedBrieFromProcessingResponse();
        AgedBrie oldItem = new AgedBrie("name", 1, 2);
        ProcessingResponse<AgedBrie> processingResponse = new ProcessingResponse<>(7, 8);
        AgedBrie newItem = agedBrieFromProcessingResponse.toQualityItem(oldItem, processingResponse);
        assertEquals(oldItem.getName(), newItem.getName());
        assertEquals(processingResponse.getSellIn(), newItem.getSellIn());
        assertEquals(processingResponse.getQuality(), newItem.getQuality());
    }
}
package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.Sulfuras;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SulfurasFromProcessingResponseTest {

    @Test
    void toQualityItem() {
        SulfurasFromProcessingResponse qualityItemFromProcessingResponse = new SulfurasFromProcessingResponse();
        Sulfuras oldItem = new Sulfuras("name", 1, 2);
        ProcessingResponse<Sulfuras> processingResponse = new ProcessingResponse<>(7, 8);
        Sulfuras newItem = qualityItemFromProcessingResponse.toQualityItem(oldItem, processingResponse);
        assertEquals(oldItem.getName(), newItem.getName());
        assertEquals(processingResponse.getSellIn(), newItem.getSellIn());
        assertEquals(processingResponse.getQuality(), newItem.getQuality());
    }
}
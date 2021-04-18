package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.NormalItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NormalItemFromProcessingResponseTest {

    @Test
    void toQualityItem() {
        NormalItemFromProcessingResponse qualityItemFromProcessingResponse = new NormalItemFromProcessingResponse();
        NormalItem oldItem = new NormalItem("name", 1, 2);
        ProcessingResponse<NormalItem> processingResponse = new ProcessingResponse<>(7, 8);
        NormalItem newItem = qualityItemFromProcessingResponse.toQualityItem(oldItem, processingResponse);
        assertEquals(oldItem.getName(), newItem.getName());
        assertEquals(processingResponse.getSellIn(), newItem.getSellIn());
        assertEquals(processingResponse.getQuality(), newItem.getQuality());
    }
}
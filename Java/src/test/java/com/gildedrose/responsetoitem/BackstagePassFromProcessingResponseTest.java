package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.BackstagePass;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BackstagePassFromProcessingResponseTest {

    @Test
    void toQualityItem() {
        BackstagePassFromProcessingResponse qualityItemFromProcessingResponse = new BackstagePassFromProcessingResponse();
        BackstagePass oldItem = new BackstagePass("name", 1, 2);
        ProcessingResponse<BackstagePass> processingResponse = new ProcessingResponse<>(7, 8);
        BackstagePass newItem = qualityItemFromProcessingResponse.toQualityItem(oldItem, processingResponse);
        assertEquals(oldItem.getName(), newItem.getName());
        assertEquals(processingResponse.getSellIn(), newItem.getSellIn());
        assertEquals(processingResponse.getQuality(), newItem.getQuality());
    }
}
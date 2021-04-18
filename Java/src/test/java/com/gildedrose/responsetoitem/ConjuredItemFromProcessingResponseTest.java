package com.gildedrose.responsetoitem;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.ConjuredItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConjuredItemFromProcessingResponseTest {

    @Test
    void toQualityItem() {
        ConjuredItemFromProcessingResponse qualityItemFromProcessingResponse = new ConjuredItemFromProcessingResponse();
        ConjuredItem oldItem = new ConjuredItem("name", 1, 2);
        ProcessingResponse<ConjuredItem> processingResponse = new ProcessingResponse<>(7, 8);
        ConjuredItem newItem = qualityItemFromProcessingResponse.toQualityItem(oldItem, processingResponse);
        assertEquals(oldItem.getName(), newItem.getName());
        assertEquals(processingResponse.getSellIn(), newItem.getSellIn());
        assertEquals(processingResponse.getQuality(), newItem.getQuality());
    }
}
package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingRequest;
import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.Sulfuras;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SulfurasProcessorTest {
    @Test
    void processItem() {
        SulfurasProcessor sulfurasProcessor = new SulfurasProcessor();
        ProcessingRequest<Sulfuras> sulfurasProcessingRequest = new ProcessingRequest<>(3, 4);
        ProcessingResponse<Sulfuras> sulfurasProcessingResponse = sulfurasProcessor.processItem(sulfurasProcessingRequest);
        assertEquals(sulfurasProcessingRequest.getSellIn(), sulfurasProcessingResponse.getSellIn());
        assertEquals(sulfurasProcessingRequest.getQuality(), sulfurasProcessingResponse.getQuality());
    }
}
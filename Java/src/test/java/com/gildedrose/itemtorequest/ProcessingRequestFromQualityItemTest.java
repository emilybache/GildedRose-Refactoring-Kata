package com.gildedrose.itemtorequest;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingRequest;
import com.gildedrose.itemsorts.NormalItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ProcessingRequestFromQualityItemTest {
    @Test
    void get() {
        ProcessingRequestFromQualityItem processingRequestFromQualityItem = new ProcessingRequestFromQualityItem();
        int sellIn = 99;
        int quality = 938;
        NormalItem normalItem = new NormalItem("name", sellIn, quality);
        ProcessingRequest<NormalItem> expectedProcessingRequest = new ProcessingRequest<>(sellIn, quality);
        assertEquals(expectedProcessingRequest, processingRequestFromQualityItem.get(normalItem));
    }
}
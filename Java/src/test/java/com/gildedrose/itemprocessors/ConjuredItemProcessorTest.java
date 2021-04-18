package com.gildedrose.itemprocessors;

import org.junit.jupiter.api.Test;

import static com.gildedrose.itemprocessors.ConjuredItemProcessor.CONJURED_ITEM_QUALITY_CHANGE_MULTIPLIER;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ConjuredItemProcessorTest {

    @Test
    void calculateQualityChange() {
        ConjuredItemProcessor conjuredItemProcessor = new ConjuredItemProcessor();
        int qualityChange = 77;
        assertEquals(qualityChange * CONJURED_ITEM_QUALITY_CHANGE_MULTIPLIER,
                conjuredItemProcessor.applyQualityChangeMultiplier(qualityChange));
    }
}
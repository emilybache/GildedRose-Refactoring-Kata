package com.gildedrose.itemprocessors;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgedBrieProcessorTest {
    @Test
    public void test(){
        AgedBrieProcessor agedBrieProcessor = new AgedBrieProcessor();
        int oldQuality = 3;
        int qualityChange = 4;
        assertEquals(oldQuality + qualityChange, agedBrieProcessor.qualityFunction(oldQuality, qualityChange));
    }
}
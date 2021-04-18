package com.gildedrose.itemprocessors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static com.gildedrose.itemprocessors.BackstagePassProcessor.FIVE_DAYS_BEFORE_CONCERT;
import static com.gildedrose.itemprocessors.BackstagePassProcessor.TEN_DAYS_BEFORE_CONCERT;
import static org.junit.jupiter.api.Assertions.assertEquals;

class BackstagePassProcessorTest {
    @Test
    public void test() {
        BackstagePassProcessor backstagePassProcessor = new BackstagePassProcessor();
        int oldQuality = 33;
        int newQuality = 88;
        assertEquals(oldQuality + newQuality, backstagePassProcessor.qualityFunction(oldQuality, newQuality));
    }

    @SuppressWarnings("unused")
    public static Stream<Arguments> calculateQualityChangeValues() {
        return Stream.of(
               Arguments.of(-1, 77, -77) ,
               Arguments.of(FIVE_DAYS_BEFORE_CONCERT - 1, 77, 3),
               Arguments.of(TEN_DAYS_BEFORE_CONCERT - 1, 77, 2),
               Arguments.of(TEN_DAYS_BEFORE_CONCERT, 77, 1)
        );
    }
    @MethodSource("calculateQualityChangeValues")
    @ParameterizedTest
    public void calculateQualityChangeTest(int sellIn, int oldQuality, int expectedQualityChange) {
        BackstagePassProcessor backstagePassProcessor = new BackstagePassProcessor();
        assertEquals(expectedQualityChange, backstagePassProcessor.calculateQualityChange(sellIn, oldQuality));
    }
}
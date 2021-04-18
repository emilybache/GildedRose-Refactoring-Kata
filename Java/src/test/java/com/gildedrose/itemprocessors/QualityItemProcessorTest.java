package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingRequest;
import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton.MAX_QUALITY;
import static com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton.MIN_QUALITY;
import static com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton.QUALITY_CHANGE_AFTER_SELL_DATE;
import static com.gildedrose.itemprocessors.QualityItemProcessor.QualityItemProcessorSkeleton.QUALITY_CHANGE_BEFORE_SELL_DATE;
import static org.junit.jupiter.api.Assertions.assertEquals;

class QualityItemProcessorTest {


    @Test
    public void testCalculateItem() {
        final int oldQualityExpected = 3;
        final int oldSellInExpected = 27;
        final int newSellInExpected = 509;
        final int newQualityExpected = 99;
        NormalItem testItem = new NormalItem("test item", oldSellInExpected, oldQualityExpected);

        ProcessingRequest<QualityItem> qualityItemProcessingRequest = new ProcessingRequest<>(testItem.getSellIn(), testItem.getQuality());
        ProcessingResponse<QualityItem> qualityItemProcessingResponse = new ProcessingResponse<>(newSellInExpected, newQualityExpected);

        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {
            @Override
            int calculateNewSellIn(int oldSellIn) {
                assertEquals(oldSellInExpected, oldSellIn);
                return newSellInExpected;
            }

            @Override
            int calculateNewQuality(int newSellIn, int oldQuality) {
                assertEquals(oldQualityExpected, oldQuality);
                assertEquals(newSellInExpected, newSellIn);
                return newQualityExpected;
            }
        };
        assertEquals(qualityItemProcessingResponse, qualityItemProcessorSkeleton.processItem(qualityItemProcessingRequest));
    }

    @Test
    public void calculateNewSellInTest() {
        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {
        };
        int oldSellIn = 1;
        assertEquals(0, qualityItemProcessorSkeleton.calculateNewSellIn(oldSellIn));
    }

    @Test
    public void calculateNewQuality() {
        int oldQualityExpected = 3;
        int newSellInExpected = 4;
        int qualityChangeExpected = 77;
        int resultOfQualityFunctionExpected = 300;
        int qualityLimitsCheckExpected = 777;
        int qualityChangeAfterQualityChangeMultiplierWasApplied = 389;
        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {
            @Override
            int calculateQualityChange(int sellIn, int oldQuality) {
                assertEquals(oldQualityExpected, oldQuality);
                assertEquals(newSellInExpected, sellIn);
                return qualityChangeExpected;
            }

            @Override
            int applyQualityChangeMultiplier(int qualityChange) {
                assertEquals(qualityChangeExpected, qualityChange);
                return qualityChangeAfterQualityChangeMultiplierWasApplied;
            }

            @Override
            int qualityFunction(int oldQuality, int qualityChange) {
                assertEquals(oldQualityExpected, oldQuality);
                assertEquals(qualityChangeAfterQualityChangeMultiplierWasApplied, qualityChange);
                return resultOfQualityFunctionExpected;
            }


            @Override
            int qualityLimitsCheck(int quality) {
                assertEquals(resultOfQualityFunctionExpected, quality);
                return qualityLimitsCheckExpected;
            }
        };

        assertEquals(qualityLimitsCheckExpected, qualityItemProcessorSkeleton.calculateNewQuality(newSellInExpected, oldQualityExpected));
    }

    @SuppressWarnings("unused")
    public static Stream<Arguments> qualityChangeTestValues() {
        return Stream.of(
                Arguments.of(-1, QUALITY_CHANGE_AFTER_SELL_DATE),
                Arguments.of(0, QUALITY_CHANGE_BEFORE_SELL_DATE)
        );
    }

    @MethodSource("qualityChangeTestValues")
    @ParameterizedTest
    public void calculateQualityChangeTest(int sellIn, int expectedChange) {
        int oldQuality = 333;
        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {};
        assertEquals(expectedChange, qualityItemProcessorSkeleton.calculateQualityChange(sellIn, oldQuality));
    }

    @Test
    void testQualityFunctionTest() {
        int oldQuality = 333;
        int qualityChange = 77;
        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {};
        assertEquals(oldQuality - qualityChange, qualityItemProcessorSkeleton.qualityFunction(oldQuality, qualityChange));
    }

    @SuppressWarnings("unused")
    public static Stream<Arguments> qualityLimitCheckValues() {
        int between_max_and_min = (MAX_QUALITY + MIN_QUALITY) / 2;
        return Stream.of(
                Arguments.of(MAX_QUALITY + 1, MAX_QUALITY),
                Arguments.of(MIN_QUALITY - 1, MIN_QUALITY),
                Arguments.of(between_max_and_min, between_max_and_min)
        );
    }

    @MethodSource("qualityLimitCheckValues")
    @ParameterizedTest
    void qualityLimitsCheckTest(int quality, int resultingQuality) {
        QualityItemProcessorSkeleton<QualityItem> qualityItemProcessorSkeleton = new QualityItemProcessorSkeleton<QualityItem>() {};
        assertEquals(resultingQuality, qualityItemProcessorSkeleton.qualityLimitsCheck(quality));
    }


}
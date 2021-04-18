package com.gildedrose.itemprocessors;

import com.gildedrose.itemsorts.QualityItem;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;


public interface QualityItemProcessor<T extends QualityItem> {
    ProcessingResponse<T> processItem(ProcessingRequest<T> processingRequest);


    @EqualsAndHashCode
    @Getter
    @AllArgsConstructor
    class ProcessingResponse<T extends QualityItem> {
        private final int sellIn;
        private final int quality;
    }

    @EqualsAndHashCode
    @Getter
    @AllArgsConstructor
    class ProcessingRequest<T extends QualityItem> {
        private final int sellIn;
        private final int quality;
    }


    /**
     * A skeleton intended for subclassing
     *
     * @param <T> - type of the item
     */
    abstract class QualityItemProcessorSkeleton<T extends QualityItem> implements QualityItemProcessor<T> {
        static final int QUALITY_CHANGE_AFTER_SELL_DATE = 2;
        static final int QUALITY_CHANGE_BEFORE_SELL_DATE = 1;
        static final int MAX_QUALITY = 50;
        static final int MIN_QUALITY = 0;
        public static final int DEFAULT_QUALITY_CHANGE_MULTIPLIER = 1;


        @Override
        public ProcessingResponse<T> processItem(ProcessingRequest<T> processingRequest) {
            int newSellIn = calculateNewSellIn(processingRequest.getSellIn());
            int newQuality = calculateNewQuality(newSellIn, processingRequest.getQuality());
            return new ProcessingResponse<>(newSellIn, newQuality);
        }

        int calculateNewSellIn(int oldSellIn) {
            return oldSellIn - 1;
        }

        int calculateNewQuality(int newSellIn, int oldQuality) {
            return qualityLimitsCheck(qualityFunction(oldQuality, applyQualityChangeMultiplier(calculateQualityChange(newSellIn, oldQuality))));
        }

        int qualityFunction(int oldQuality, int qualityChange) {
            return oldQuality - qualityChange;
        }

        int applyQualityChangeMultiplier(int qualityChange) {
            return DEFAULT_QUALITY_CHANGE_MULTIPLIER * qualityChange;
        }

        int calculateQualityChange(int sellIn, int oldQuality) {
            if (sellIn < 0) {
                return QUALITY_CHANGE_AFTER_SELL_DATE;
            } else {
                return QUALITY_CHANGE_BEFORE_SELL_DATE;
            }
        }


        int qualityLimitsCheck(int quality) {
            if (quality > MAX_QUALITY) {
                return MAX_QUALITY;
            } else if (quality < MIN_QUALITY) {
                return MIN_QUALITY;
            } else {
                return quality;
            }
        }
    }

}

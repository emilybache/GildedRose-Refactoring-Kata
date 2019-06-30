package com.gildedrose.rules;

import static java.lang.Integer.max;

public class QualityRule {

    public boolean shouldApply(String itemName) {
        return true;
    }

    public Result calculateQuality(int oldQuality) {
        return new Result(max(oldQuality - 1, 0), false);
    }

    public static class Result {
        final int quality;
        final boolean isFinalValue;

        public Result(int quality, boolean isFinalValue) {
            this.quality = quality;
            this.isFinalValue = isFinalValue;
        }

        public int getQuality() {
            return quality;
        }

        public boolean isFinalValue() {
            return isFinalValue;
        }
    }
}

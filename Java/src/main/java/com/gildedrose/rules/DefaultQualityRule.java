package com.gildedrose.rules;

import static java.lang.Integer.max;

public class DefaultQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return true;
    }

    @Override
    public Result calculateQuality(int oldQuality, int sellIn) {
        final int newQuality;
        if (sellIn < 0) {
            newQuality = oldQuality - 2;
        } else {
            newQuality = oldQuality - 1;
        }
        return new Result(max(newQuality, 0), false);
    }

}

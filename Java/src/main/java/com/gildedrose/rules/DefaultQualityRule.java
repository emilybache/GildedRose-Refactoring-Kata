package com.gildedrose.rules;

import static java.lang.Integer.max;

public class DefaultQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return true;
    }

    @Override
    public Result calculateQuality(int oldQuality, int newSellIn) {
        final int newQuality;
        if (newSellIn < 0) {
            newQuality = oldQuality - 2;
        } else {
            newQuality = oldQuality - 1;
        }
        return new Result(max(newQuality, 0), false);
    }

}

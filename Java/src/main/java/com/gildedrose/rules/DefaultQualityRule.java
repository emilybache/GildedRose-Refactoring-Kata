package com.gildedrose.rules;

import static java.lang.Integer.max;

public class DefaultQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return true;
    }

    @Override
    public Result calculateQuality(int oldQuality, int sellIn) {
        return new Result(max(oldQuality - 1, 0), false);
    }

}

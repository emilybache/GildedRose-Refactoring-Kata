package com.gildedrose.rules;

import static java.lang.Integer.min;

public class AgedBrieQualityRule extends QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return "Aged Brie".equals(itemName);
    }

    @Override
    public Result calculateQuality(int oldQuality) {
        return new Result(min(oldQuality + 1, 50), true);
    }
}

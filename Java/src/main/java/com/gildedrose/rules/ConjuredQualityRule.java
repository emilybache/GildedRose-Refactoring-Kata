package com.gildedrose.rules;

import static java.lang.Integer.max;

public class ConjuredQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return itemName != null && itemName.startsWith("Conjured ");
    }

    @Override
    public Result calculateQuality(int oldQuality, int newSellIn) {
        return new Result(max(oldQuality - 2, 0), true);
    }
}

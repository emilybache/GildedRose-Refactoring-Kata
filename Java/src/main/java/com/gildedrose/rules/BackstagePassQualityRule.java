package com.gildedrose.rules;

import static java.lang.Integer.min;

public class BackstagePassQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return "Backstage passes to a TAFKAL80ETC concert".equals(itemName);
    }

    @Override
    public Result calculateQuality(int oldQuality, int sellIn) {
        final int newQuality = oldQuality + 2;
        return new Result(min(newQuality, 50), true);
    }
}

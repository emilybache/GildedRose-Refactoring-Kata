package com.gildedrose.rules;

import static java.lang.Integer.min;

public class BackstagePassQualityRule implements QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return "Backstage passes to a TAFKAL80ETC concert".equals(itemName);
    }

    @Override
    public Result calculateQuality(int oldQuality, int sellIn) {
        final int newQuality;

        if (sellIn < 0) {
            newQuality = 0;
        } else if (sellIn < 5) {
            newQuality = oldQuality + 3;
        } else if (sellIn < 10) {
            newQuality = oldQuality + 2;
        } else {
            newQuality = oldQuality + 1;
        }
        return new Result(min(newQuality, 50), true);
    }
}

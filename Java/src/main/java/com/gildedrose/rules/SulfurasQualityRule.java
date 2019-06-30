package com.gildedrose.rules;

public class SulfurasQualityRule extends QualityRule {

    @Override
    public boolean shouldApply(String itemName) {
        return "Sulfuras, Hand of Ragnaros".equals(itemName);
    }

    @Override
    public Result calculateQuality(int oldQuality) {
        return new Result(oldQuality, true);
    }
}

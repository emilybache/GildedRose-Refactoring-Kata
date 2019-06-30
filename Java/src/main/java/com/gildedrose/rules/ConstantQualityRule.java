package com.gildedrose.rules;

import java.util.Objects;

public class ConstantQualityRule implements QualityRule {

    private final String expectedName;

    public ConstantQualityRule(String itemName) {
        this.expectedName = Objects.requireNonNull(itemName, "ItemName is required");
    }

    @Override
    public boolean shouldApply(String itemName) {
        return expectedName.equals(itemName);
    }

    @Override
    public Result calculateQuality(int oldQuality, int newSellIn) {
        return new Result(oldQuality, true);
    }
}

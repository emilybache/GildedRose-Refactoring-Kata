package com.gildedrose.rules;

import static java.lang.Integer.max;
import static java.lang.Integer.min;

public class DefaultQualityRule implements QualityRule {

    private final int multiplier;
    private final String expectedItemName;
    private final boolean isFinalRule;

    public DefaultQualityRule() {
        this(null, 1, false);
    }

    public DefaultQualityRule(String itemName, int multiplier, boolean isFinalRule) {
        this.multiplier = multiplier;
        this.expectedItemName = itemName;
        this.isFinalRule = isFinalRule;
    }

    @Override
    public boolean shouldApply(String itemName) {
        if (expectedItemName != null) {
            return expectedItemName.equals(itemName);
        } else {
            return true;
        }
    }

    @Override
    public Result calculateQuality(int oldQuality, int newSellIn) {
        final int newQuality;
        if (newSellIn < 0) {
            newQuality = oldQuality - 2 * multiplier;
        } else {
            newQuality = oldQuality - multiplier;
        }
        return new Result(min(max(newQuality, 0), 50), isFinalRule);
    }

}

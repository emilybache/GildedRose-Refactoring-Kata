package com.gildedrose.rules;

public class ConjuredQualityRule extends DefaultQualityRule {

    public ConjuredQualityRule() {
        super(null, 2, true);
    }

    @Override
    public boolean shouldApply(String itemName) {
        return itemName != null && itemName.startsWith("Conjured ");
    }
}

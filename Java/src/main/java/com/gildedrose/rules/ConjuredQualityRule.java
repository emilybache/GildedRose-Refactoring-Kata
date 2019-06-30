package com.gildedrose.rules;

public class ConjuredQualityRule extends DefaultQualityRule {

    public ConjuredQualityRule() {
        super(2, null, true);
    }

    @Override
    public boolean shouldApply(String itemName) {
        return itemName != null && itemName.startsWith("Conjured ");
    }
}

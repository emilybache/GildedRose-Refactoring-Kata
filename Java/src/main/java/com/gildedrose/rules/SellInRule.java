package com.gildedrose.rules;

public interface SellInRule {

    boolean shouldApply(String itemName);

    Result calculateQuality(int oldQuality, int newSellIn);
}

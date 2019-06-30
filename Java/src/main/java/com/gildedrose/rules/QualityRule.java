package com.gildedrose.rules;

public interface QualityRule {
    boolean shouldApply(String itemName);

    Result calculateQuality(int oldQuality, int sellIn);
}

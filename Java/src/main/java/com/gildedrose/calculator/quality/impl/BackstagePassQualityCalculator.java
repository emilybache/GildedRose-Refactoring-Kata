package com.gildedrose.calculator.quality.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.QualityCalculator;

public class BackstagePassQualityCalculator implements QualityCalculator {
    @Override
    public int calculate(Item item, int updatedSellIn) {
        if (updatedSellIn < 0) {
            return 0;
        }

        int qualityIncrease = 1 + (item.sellIn < 11 ? 1 : 0) + (item.sellIn < 6 ? 1 : 0);
        int newQuality = item.quality + qualityIncrease;

        return Math.min(50, newQuality);
    }
}

package com.gildedrose.calculator.quality.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.QualityCalculator;

public class AgedBrieQualityCalculator implements QualityCalculator {
    @Override
    public int calculate(Item item, int updatedSellIn) {
        int increaseAmount = (updatedSellIn < 0) ? 2 : 1;
        int newQuality = item.quality + increaseAmount;
        return Math.min(50, newQuality);
    }
}

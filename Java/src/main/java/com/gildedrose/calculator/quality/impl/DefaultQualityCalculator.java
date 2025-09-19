package com.gildedrose.calculator.quality.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.QualityCalculator;

public class DefaultQualityCalculator implements QualityCalculator {
    @Override
    public int calculate(Item item, int updatedSellIn) {
        int decreaseAmount = (updatedSellIn < 0) ? 2 : 1;
        int newQuality = item.quality - decreaseAmount;
        return Math.max(0, newQuality);
    }
}

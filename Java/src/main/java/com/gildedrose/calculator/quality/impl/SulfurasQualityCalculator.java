package com.gildedrose.calculator.quality.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.QualityCalculator;

public class SulfurasQualityCalculator implements QualityCalculator {
    @Override
    public int calculate(Item item, int updatedSellIn) {
        return item.quality;
    }
}

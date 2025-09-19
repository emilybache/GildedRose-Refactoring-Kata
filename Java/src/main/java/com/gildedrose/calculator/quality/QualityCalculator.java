package com.gildedrose.calculator.quality;

import com.gildedrose.Item;

public interface QualityCalculator {
    int calculate(Item item, int updatedSellIn);
}

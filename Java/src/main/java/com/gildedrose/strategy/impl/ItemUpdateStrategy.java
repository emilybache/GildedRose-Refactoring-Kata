package com.gildedrose.strategy.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.QualityCalculator;
import com.gildedrose.calculator.sellIn.SellInCalculator;
import com.gildedrose.strategy.UpdateStrategy;

public class ItemUpdateStrategy implements UpdateStrategy {

    private final QualityCalculator qualityCalculator;
    private final SellInCalculator sellinCalculator;

    public ItemUpdateStrategy(QualityCalculator qualityCalculator, SellInCalculator sellinCalculator) {
        this.qualityCalculator = qualityCalculator;
        this.sellinCalculator = sellinCalculator;
    }

    @Override
    public void update(Item item) {
        int updatedSellIn = this.sellinCalculator.calculate(item);
        int updatedQuality = this.qualityCalculator.calculate(item, updatedSellIn);
        item.sellIn = updatedSellIn;
        item.quality = updatedQuality;
    }
}

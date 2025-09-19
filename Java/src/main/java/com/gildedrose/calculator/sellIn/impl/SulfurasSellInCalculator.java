package com.gildedrose.calculator.sellIn.impl;

import com.gildedrose.Item;
import com.gildedrose.calculator.sellIn.SellInCalculator;

public class SulfurasSellInCalculator implements SellInCalculator {
    @Override
    public int calculate(Item item) {
        return item.sellIn;
    }
}

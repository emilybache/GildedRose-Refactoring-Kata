package com.gildedrose.rules;

public class DefaultAgingStrategy implements AgingStrategy {
    @Override
    public int calculateSellIn(int oldSellIn) {
        return oldSellIn - 1;
    }
}

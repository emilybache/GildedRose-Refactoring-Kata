package com.gildedrose.rules;

public class NoAgingStrategy implements AgingStrategy {

    @Override
    public int calculateSellIn(int oldSellIn) {
        return oldSellIn;
    }
}

package com.gildedrose.strategies;

import com.gildedrose.Item;

public enum SulfurasStrategy implements QualityUpdateStrategy{
    INSTANCE;

    @Override
    public void applyTo(Item item) {
        //do nothing
    }
}

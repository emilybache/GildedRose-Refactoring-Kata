package com.gildedrose.strategies;

import com.gildedrose.Item;

public interface QualityUpdateStrategy {
    void applyTo(Item item);
}

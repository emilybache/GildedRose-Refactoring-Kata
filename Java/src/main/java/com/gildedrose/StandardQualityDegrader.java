package com.gildedrose;

import java.util.function.ToIntFunction;

public class StandardQualityDegrader implements ToIntFunction<Item> {

    private final int speed;

    public StandardQualityDegrader() {
        this(1);
    }

    public StandardQualityDegrader(int speed) {
        this.speed = speed;
    }

    @Override
    public int applyAsInt(Item item) {
        // Once the sell by date has passed, Quality degrades twice as fast:
        return item.sellIn >= 0 ? item.quality - speed : item.quality - speed * 2;
    }
}

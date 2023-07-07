package com.gildedrose;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items).forEach(this::updateQuality);
    }

    public void updateQuality(Item item) {
        if (item.name.equals("Aged Brie")) {
            AgedBrieStrategy.INSTANCE.applyTo(item);
        } else if (item.name.startsWith("Backstage passes")) {
            BackstagePassesStrategy.INSTANCE.applyTo(item);
        } else if (item.name.startsWith("Conjured")) {
            ConjuredStrategy.INSTANCE.applyTo(item);
        } else {
            DefaultStrategy.INSTANCE.applyTo(item);
        }
    }
}

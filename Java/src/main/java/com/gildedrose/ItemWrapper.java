package com.gildedrose;

import com.gildedrose.strategies.AgedBrieStrategy;
import com.gildedrose.strategies.BackstagePassesStrategy;
import com.gildedrose.strategies.ConjuredStrategy;
import com.gildedrose.strategies.DefaultStrategy;
import com.gildedrose.strategies.QualityUpdateStrategy;
import com.gildedrose.strategies.SulfurasStrategy;

public class ItemWrapper {
    private final Item item;
    private final QualityUpdateStrategy strategy;

    public ItemWrapper(Item item) {
        this.item = item;
        if (item.name.equals("Aged Brie")) {
            strategy = AgedBrieStrategy.INSTANCE;
        } else if (item.name.startsWith("Backstage passes")) {
            strategy = BackstagePassesStrategy.INSTANCE;
        } else if (item.name.startsWith("Conjured")) {
            strategy = ConjuredStrategy.INSTANCE;
        } else if (item.name.startsWith("Sulfuras")) {
            strategy = SulfurasStrategy.INSTANCE;
        } else {
            strategy = DefaultStrategy.INSTANCE;
        }
    }

    public void updateQuality() {
        strategy.applyTo(item);
    }
}

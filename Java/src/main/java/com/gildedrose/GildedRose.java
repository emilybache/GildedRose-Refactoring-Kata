package com.gildedrose;

import com.gildedrose.strategy.*;

import java.util.HashMap;

class GildedRose {
    public static final String DEFAULT_STRATEGY = "default";
    Item[] items;
    HashMap<String, ItemUpdateStrategy> strategies;

    public GildedRose(Item[] items) {
        this.items = items;
        this.strategies = new HashMap<>();
        this.strategies.put("Aged Brie", new AgedBrieUpdateStrategy());
        this.strategies.put("Backstage passes to a TAFKAL80ETC concert", new BackstagePassesUpdateStrategy());
        this.strategies.put("Sulfuras, Hand of Ragnaros", new SulfurasUpdateStrategy());
        this.strategies.put(DEFAULT_STRATEGY, new StandardItemUpdateStrategy());
    }

    public void updateQuality() {
        for (Item item : items) {
            getItemUpdateStrategy(item).update(item);
        }
    }

    private ItemUpdateStrategy getItemUpdateStrategy(Item item) {
        return strategies.getOrDefault(item.name, strategies.get(DEFAULT_STRATEGY));
    }
}

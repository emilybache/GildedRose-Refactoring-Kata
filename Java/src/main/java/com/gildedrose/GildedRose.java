package com.gildedrose;

import com.gildedrose.strategy.AgedBrieItemStrategyImpl;
import com.gildedrose.strategy.BackStageItemStrategyImpl;
import com.gildedrose.strategy.ConjuredItemStrategyImpl;
import com.gildedrose.strategy.ItemStrategy;
import com.gildedrose.strategy.LegendaryItemStrategyImpl;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

class GildedRose {
    static final ItemStrategy DEFAULT_STRATEGY = new ItemStrategy() {
    };
    private static final Map<String, ItemStrategy> STRATEGIES = new TreeMap<>();

    static final String AGED_BRIE = "Aged Brie";
    static final String CONJURED = "Conjured Mana Cake";
    static final String DEXTERITY_VEST = "+5 Dexterity Vest";
    static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    static final String ELIXIR_OF_THE_MONGOOSE = "Elixir of the Mongoose";
    static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";

    private static final List<String> KNOWN_ITEM_NAMES =
        List.of(AGED_BRIE, CONJURED, DEXTERITY_VEST, SULFURAS, ELIXIR_OF_THE_MONGOOSE,
            BACKSTAGE_PASSES);

    static {
        STRATEGIES.put(DEXTERITY_VEST, DEFAULT_STRATEGY);
        STRATEGIES.put(ELIXIR_OF_THE_MONGOOSE, DEFAULT_STRATEGY);

        STRATEGIES.put(AGED_BRIE, new AgedBrieItemStrategyImpl());
        STRATEGIES.put(CONJURED, new ConjuredItemStrategyImpl());
        STRATEGIES.put(BACKSTAGE_PASSES, new BackStageItemStrategyImpl());

        STRATEGIES.put(SULFURAS, new LegendaryItemStrategyImpl());
    }

    private final Item[] items;

    public GildedRose(Item[] items) {
        if (items == null || items.length == 0) {
            throw new IllegalArgumentException("Items cannot be empty");
        }
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items)
            .forEach(item -> GildedRose.findStrategy(item.name).updateQuality(item));
    }

    static ItemStrategy findStrategy(String itemName) {
        if (KNOWN_ITEM_NAMES.contains(itemName)) {
            return STRATEGIES.get(itemName);
        }
        return DEFAULT_STRATEGY;
    }

    public Item[] getItems() {
        return items;
    }
}

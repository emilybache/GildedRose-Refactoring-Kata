package com.gildedrose;

import com.gildedrose.rules.AgingStrategy;
import com.gildedrose.rules.BackstagePassQualityRule;
import com.gildedrose.rules.ConjuredQualityRule;
import com.gildedrose.rules.ConstantQualityRule;
import com.gildedrose.rules.DefaultAgingStrategy;
import com.gildedrose.rules.DefaultQualityRule;
import com.gildedrose.rules.NoAgingStrategy;
import com.gildedrose.rules.QualityRule;
import com.gildedrose.rules.Result;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

class GildedRose {

    @SuppressWarnings("SpellCheckingInspection")
    private static final String SULFURAS_HAND_OF_RAGNAROS = "Sulfuras, Hand of Ragnaros";

    final Item[] items;

    private final List<QualityRule> rules = List.of(
            new ConstantQualityRule(SULFURAS_HAND_OF_RAGNAROS),
            new DefaultQualityRule("Aged Brie", -1, true),
            new BackstagePassQualityRule(),
            new ConjuredQualityRule(),
            new DefaultQualityRule()
    );

    private final DefaultAgingStrategy defaultAgingStrategy = new DefaultAgingStrategy();
    private final Map<String, AgingStrategy> nameToAgingStrategyMap = Map.of(
            SULFURAS_HAND_OF_RAGNAROS, new NoAgingStrategy()
    );

    @SuppressWarnings("WeakerAccess")
    public GildedRose(Item[] items) {
        this.items = items;
    }

    @SuppressWarnings("WeakerAccess")
    public void updateQuality() {
        Arrays.stream(items)
                .parallel()
                .forEach(this::processItem);
    }

    private void processItem(final Item item) {
        item.sellIn = calculateSellIn(item.name, item.sellIn);

        item.quality = calculateQuality(item.name, item.quality, item.sellIn);
    }

    private int calculateSellIn(final String name, final int originalSellIn) {
        return nameToAgingStrategyMap
                .getOrDefault(name, defaultAgingStrategy)
                .calculateSellIn(originalSellIn);
    }

    private int calculateQuality(String itemName, int quality, int newSellIn) {
        return rules.stream()
                .reduce(new Result(quality, false),
                        (q, rule) -> {
                            if (q.isFinalValue() || !rule.shouldApply(itemName)) {
                                return q;
                            } else {
                                return rule.calculateQuality(q.getQuality(), newSellIn);
                            }
                        },
                        (a, b) -> b).getQuality();
    }
}

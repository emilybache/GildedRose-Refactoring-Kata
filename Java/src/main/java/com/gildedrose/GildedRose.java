package com.gildedrose;

import com.gildedrose.rules.AgedBrieQualityRule;
import com.gildedrose.rules.BackstagePassQualityRule;
import com.gildedrose.rules.ConjuredQualityRule;
import com.gildedrose.rules.DefaultQualityRule;
import com.gildedrose.rules.QualityRule;
import com.gildedrose.rules.Result;
import com.gildedrose.rules.SulfurasQualityRule;

import java.util.Arrays;
import java.util.List;

class GildedRose {
    Item[] items;

    private final List<QualityRule> rules = List.of(
            new SulfurasQualityRule(),
            new AgedBrieQualityRule(),
            new BackstagePassQualityRule(),
            new ConjuredQualityRule(),
            new DefaultQualityRule()
    );

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items)
                .parallel()
                .forEach(this::processItem);
    }

    private void processItem(final Item item) {

        item.sellIn = calculateSellIn(item.name, item.sellIn);

        item.quality = rules.stream()
                .reduce(new Result(item.quality, false),
                        (q, rule) -> {
                            if (q.isFinalValue() || !rule.shouldApply(item.name)) {
                                return q;
                            } else {
                                return rule.calculateQuality(q.getQuality(), item.sellIn);
                            }
                        },
                        (a, b) -> b).getQuality();

    }

    private int calculateSellIn(final String name, final int originalSellIn) {
        if (!name.equals("Sulfuras, Hand of Ragnaros")) {
            return originalSellIn - 1;
        } else {
            return originalSellIn;
        }

    }
}

package com.gildedrose;

import com.gildedrose.rules.AgedBrieQualityRule;
import com.gildedrose.rules.BackstagePassQualityRule;
import com.gildedrose.rules.DefaultQualityRule;
import com.gildedrose.rules.QualityRule;
import com.gildedrose.rules.Result;
import com.gildedrose.rules.SulfurasQualityRule;
import lombok.val;

import java.util.List;

class GildedRose {
    Item[] items;

    private final List<QualityRule> rules = List.of(
            new SulfurasQualityRule(),
            new AgedBrieQualityRule(),
            new BackstagePassQualityRule(),
            new DefaultQualityRule()
    );

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            final Item item = items[i];
            processItem(item);
        }
    }

    private void processItem(final Item item) {

        val newSellIn = calculateSellIn(item.name, item.sellIn);

        val newQuality = rules.stream()
                .reduce(new Result(item.quality, false),
                        (q, rule) -> {
                            if (q.isFinalValue() || !rule.shouldApply(item.name)) {
                                return q;
                            } else {
                                return rule.calculateQuality(q.getQuality(), newSellIn);
                            }
                        },
                        (a, b) -> b).getQuality();

        item.quality = newQuality;
        item.sellIn = newSellIn;
    }

    private int calculateSellIn(final String name, final int originalSellIn) {
        if (!name.equals("Sulfuras, Hand of Ragnaros")) {
            return originalSellIn - 1;
        } else {
            return originalSellIn;
        }

    }
}

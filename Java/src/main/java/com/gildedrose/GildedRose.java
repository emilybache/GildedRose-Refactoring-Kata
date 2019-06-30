package com.gildedrose;

import com.gildedrose.rules.AgedBrieQualityRule;
import com.gildedrose.rules.QualityRule;
import com.gildedrose.rules.SulfurasQualityRule;
import lombok.val;

import java.util.List;

class GildedRose {
    Item[] items;

    private final List<QualityRule> rules = List.of(
            new SulfurasQualityRule(),
            new AgedBrieQualityRule(),
            new QualityRule()
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

        var newQuality = rules.stream()
                .reduce(new QualityRule.Result(item.quality, false),
                        (q, rule) -> {
                            if (q.isFinalValue() || !rule.shouldApply(item.name)) {
                                return q;
                            } else {
                                return rule.calculateQuality(q.getQuality());
                            }
                        },
                        (a, b) -> b).getQuality();

/*
        if (!item.name.equals("Aged Brie")
                && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (item.quality > 0) {
                if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                    item.quality = item.quality - 1;
                }
            }
        } else {
            if (item.quality < 50) {
                item.quality = item.quality + 1;

                if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    if (item.sellIn < 11) {
                        if (item.quality < 50) {
                            item.quality = item.quality + 1;
                        }
                    }

                    if (item.sellIn < 6) {
                        if (item.quality < 50) {
                            item.quality = item.quality + 1;
                        }
                    }
                }
            }
        }
*/

        val newSellIn = calculateSellIn(item.name, item.sellIn);

/*        if (newSellIn < 0) {
            if (!item.name.equals("Aged Brie")) {
                if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    if (item.quality > 0) {
                        if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                            item.quality = item.quality - 1;
                        }
                    }
                } else {
                    item.quality = item.quality - item.quality;
                }
            } else {
                if (item.quality < 50) {
                    item.quality = item.quality + 1;
                }
            }
        }*/

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

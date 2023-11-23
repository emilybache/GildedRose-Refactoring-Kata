package com.gildedrose;

import static com.gildedrose.Inventory.BACKSTAGE_PASS;
import static com.gildedrose.Inventory.LEGENDARY;
import static com.gildedrose.service.InventoryHelper.*;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.of;

class GildedRose {
    Item[] items;

    GildedRose(Item[] items) {
        this.items = items;
    }

    void updateQuality() {
        of(items).forEach(item -> {
            // legendary items should not be sold
            if (itemNotLegendary(item)) item.sellIn--;

            // increase quality when quality decrease is inverted
            if (includesItems(item, getInventoriesWithInvertedQualityDecrease())) {
                increaseQualityBelowMaximum(item);

                // increase backstage passes
                increaseBackstagePass(item);
            }
            // decrease average (non-legendary) items
            else if (itemNotLegendary(item)) {
                // decrease quality based on their decrease amount
                range(0, getQualityDecreaseAmount(item)).forEach(i -> decreaseQualityAboveZero(item));
            }

            if (item.sellIn < 0) {
                // increase quality when aged brie
                if (itemAgedBrie(item)) increaseQualityBelowMaximum(item);

                    // when not aged brie, backstage pass or legendary, decrease quality above zero
                else decreaseQualityAboveZeroItemsOtherThan(item, BACKSTAGE_PASS, LEGENDARY);
            }
        });
    }
}

package com.gildedrose.domain;

import com.gildedrose.Item;

public class BackstagePass extends InventoryItem {

    public BackstagePass(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public int handleQuality() {
        return increaseQualityBelowMaximum();
    }

    @Override
    public int qualityDecreaseAmount() {
        return 1;
    }

    @Override
    public int handleQualityAfterSellIn() {
        return 0;
    }

    @Override
    public int increaseQualityBelowMaximum() {
        increaseQualityIfNotMaximum();

        // increase backstage pass further when sellIn date approaches
        return increaseBackstagePass();
    }

    private int increaseBackstagePass() {
        if (sellIn < 10) {
            increaseQualityIfNotMaximum();

            if (sellIn < 5) {
                increaseQualityIfNotMaximum();
            }
        }
        return quality;
    }

    private void increaseQualityIfNotMaximum() {
        quality = super.increaseQualityBelowMaximum();
    }
}

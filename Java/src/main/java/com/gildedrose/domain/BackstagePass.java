package com.gildedrose.domain;

import com.gildedrose.Item;
import com.gildedrose.service.InventoryItem;

public class BackstagePass extends InventoryItem {

    public BackstagePass(Item item) {
        setName(item.name);
        setSellIn(item.sellIn);
        setQuality(item.quality);
    }

    @Override
    public boolean qualityDecreaseInverted() {
        return true;
    }

    @Override
    public int qualityDecreaseAmount() {
        return 1;
    }

    @Override
    public int handleQualityAfterSellIn() {
        quality = 0;
        return quality;
    }

    @Override
    public int increaseQualityBelowMaximum() {
        quality = increaseQualityIfNotMaximum();

        // increase backstage pass further when sellIn date approaches
        quality = increaseBackstagePass();
        return quality;
    }

    private int increaseBackstagePass() {
        if (sellIn < 10) {
            quality = increaseQualityIfNotMaximum();

            if (sellIn < 5) {
                quality = increaseQualityIfNotMaximum();
            }
        }
        return quality;
    }

    private int increaseQualityIfNotMaximum() {
        return super.increaseQualityBelowMaximum();
    }
}

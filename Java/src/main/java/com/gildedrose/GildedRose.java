package com.gildedrose;

import com.gildedrose.SellInUpdater;
import com.gildedrose.QualityUpdater;

class GildedRose {
    Item[] items;
    SellInUpdater sellInUpdater = new SellInUpdater();
    QualityUpdater qualityUpdater = new QualityUpdater();

    public GildedRose(Item[] items) {
        this.items = items;
    }

    //For each item update the SellIn value and adjust quality accordingly
    public void updateQuality() throws Exception {
        for (Item item : items) {
            sellInUpdater.updateSellInValue(item);
            qualityUpdater.updateQualityForItem(item);
        }
    }

}
package com.gildedrose;

class GildedRose {
    Item[] items;

    private final QualityUpdater qualityUpdater = new QualityUpdater();

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        qualityUpdater.updateQuality(items);
    }
}

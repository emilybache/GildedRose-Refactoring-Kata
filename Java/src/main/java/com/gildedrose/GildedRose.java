package com.gildedrose;

class GildedRose {
    UpdatableItem[] items;

    public GildedRose(Item[] items) {
        this.items = new UpdatableItem[items.length];
        for (int i = 0; i < items.length; i++) {
            this.items[i] = wrapItem(items[i]);
        }
    }

    private UpdatableItem wrapItem(Item item) {
        switch (item.name) {
            case "Aged Brie":
                return new AgedBrieItem(item.sellIn, item.quality);
            case "Backstage passes to a TAFKAL80ETC concert":
                return new BackstagePassItem(item.sellIn, item.quality);
            case "Sulfuras, Hand of Ragnaros":
                return new SulfurasItem(item.sellIn, item.quality);
            default:
                return new NormalItem(item.name, item.sellIn, item.quality);
        }
    }

    public void updateQuality() {
        for (UpdatableItem item : items) {
            item.update();
        }
    }
}

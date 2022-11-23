package com.gildedrose;

public class StorageItem {
    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    protected Item item;

    public static StorageItem createItem(Item item) {
        if (item.name.equals(AGED_BRIE)) {
            return new AgeddBrie(item);
        }
        if (item.name.equals(BACKSTAGE_PASSES)) {
            return new BackstagePasses(item);
        }
        if (item.name.equals(SULFURAS)) {
            return new Sulfuras(item);
        }
        return new StorageItem(item);
    }

    public StorageItem(Item item) {
        this.item = item;
    }

    public void dailyUpdateItem() {
        updateQuality();
        updateSelling();
        if (isExpired()) {
            updateExpired();
        }
    }

    protected void updateQuality() {
        decreaseQuality();
    }

    protected void updateSelling() {
        item.sellIn--;
    }

    protected boolean isExpired() {
        return item.sellIn < 0;
    }

    protected void updateExpired() {
        decreaseQuality();
    }

    protected void increaseQuality() {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    protected void decreaseQuality() {
        if (item.quality > 0) {
            item.quality--;
        }
    }
}

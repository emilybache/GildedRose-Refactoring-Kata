package com.gildedrose;

public class StorageItem {
    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private Item item;

    public static StorageItem createItem(Item item) {
        if (item.name.equals(AGED_BRIE)){
            return new AgeddBrie(item);
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
        if (item.name.equals(BACKSTAGE_PASSES)) {
            increaseQuality();

            if (item.sellIn < 11) {
                increaseQuality();
            }

            if (item.sellIn < 6) {
                increaseQuality();
            }
        } else if (item.name.equals(SULFURAS)) {
            return;
        } else decreaseQuality();
    }

    protected void updateSelling() {
        if (item.name.equals(SULFURAS)) {
            return;
        }
        item.sellIn--;
    }

    protected boolean isExpired() {
        return item.sellIn < 0;
    }

    protected void updateExpired() {
        if (item.name.equals(BACKSTAGE_PASSES)) {
            item.quality = 0;
        } else if (item.name.equals(SULFURAS)) {
            return;
        } else {
            decreaseQuality();
        }
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

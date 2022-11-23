package com.gildedrose;

public class StorageItem {
    protected Item item;

    public static StorageItem createItem(Item item) {
        switch (item.name) {
            case AgeddBrie.NAME:
                return new AgeddBrie(item);
            case BackstagePasses.NAME:
                return new BackstagePasses(item);
            case Sulfuras.NAME:
                return new Sulfuras(item);
            case Conjured.NAME:
                return new Conjured(item);
            case default:
                return new StorageItem(item);
        }
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

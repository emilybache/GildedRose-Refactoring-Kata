package com.gildedrose;

public class StorageItem {
    protected Item item;

    public static StorageItem createItem(Item item) {
        if (item.name.equals(AgeddBrie.NAME)) {
            return new AgeddBrie(item);
        }
        if (item.name.equals(BackstagePasses.NAME)) {
            return new BackstagePasses(item);
        }
        if (item.name.equals(Sulfuras.NAME)) {
            return new Sulfuras(item);
        }
        if (item.name.equals(Conjured.NAME)) {
            return new Conjured(item);
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

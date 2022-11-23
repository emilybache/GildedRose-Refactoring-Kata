package com.gildedrose;

public class StrorageItem {
    private Item item;

    public StrorageItem(Item item) {
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
        if (item.name.equals("Aged Brie")) {
            increaseQuality();
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            increaseQuality();

            if (item.sellIn < 11) {
                increaseQuality();
            }

            if (item.sellIn < 6) {
                increaseQuality();
            }
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else decreaseQuality();
    }

    protected void updateSelling() {
        if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        }
        item.sellIn--;
    }

    protected boolean isExpired() {
        return item.sellIn < 0;
    }

    protected void updateExpired() {
        if (item.name.equals("Aged Brie")) {
            increaseQuality();
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            item.quality = 0;
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
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

package com.gildedrose;

class GildedRose {
    Item[] items;
    String SULFURA = "Sulfuras, Hand of Ragnaros";
    String AGED_BRIE = "Aged Brie";
    String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";

    public GildedRose(Item[] items) {
        this.items = items;
    }

    private void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }

    private void updateNumberOfdayToSellRemaining(Item item) {
        item.sellIn -= 1;
    }

    private boolean itemHasExpired(Item item) {
        boolean condition;
        if (item.sellIn < 0) {
            condition = true;
        } else {
            condition = false;
        }
        return condition;
    }

    private void decreaseQualityTwice(Item item) {
        decreaseQuality(item);
        decreaseQuality(item);
    }

    private void updateAgedBrieQuaility(Item item) {
        increaseQuality(item);
        if (itemHasExpired(item)) {
            increaseQuality(item);
        }
    }

    private void updateBackstageQuaility(Item item) {
        increaseQuality(item);
        if (item.sellIn < 10) {
            increaseQuality(item);
        }
        if (item.sellIn < 5) {
            increaseQuality(item);
        }
        if (itemHasExpired(item)) {
            item.quality -= item.quality;
        }
    }

    private void updateQualityItem(Item item) {
        if (item.name.equals(AGED_BRIE)) {
            updateAgedBrieQuaility(item);
        } else if (item.name.equals(BACKSTAGE)) {
            updateBackstageQuaility(item);
        } else {
            if (itemHasExpired(item)) {
                decreaseQualityTwice(item);
            } else {
                decreaseQuality(item);
            }
        }
    }

    public void updateQuality() {
        for (Item item : items) {
            if (item.name.equals(SULFURA)) {continue;}
            updateNumberOfdayToSellRemaining(item);
            updateQualityItem(item);
        }
    }
}

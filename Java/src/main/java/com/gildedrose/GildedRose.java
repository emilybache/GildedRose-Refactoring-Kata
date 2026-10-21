package com.gildedrose;

class GildedRose {
    private static final String AGED_BRIE = "Aged Brie";
    private static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private static final String CONJURED = "Conjured Mana Cake";
    private static final int MIN_QUALITY = 0;
    private static final int MAX_QUALITY = 50;
    private static final int BACKSTAGE_DOUBLE_INCREASE_DAYS = 10;
    private static final int BACKSTAGE_TRIPLE_INCREASE_DAYS = 5;

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            updateItem(item);
        }
    }

    private void updateItem(Item item) {
        switch (item.name) {
            case SULFURAS:
                break;
            case AGED_BRIE:
                updateAgedBrie(item);
                break;
            case BACKSTAGE_PASSES:
                updateBackstagePass(item);
                break;
            case CONJURED:
                updateConjuredItem(item);
                break;
            default:
                updateNormalItem(item);
        }
    }

    private void updateNormalItem(Item item) {
        decreaseQuality(item);
        item.sellIn--;
        if (isExpired(item)) {
            decreaseQuality(item);
        }
    }

    private void updateAgedBrie(Item item) {
        increaseQuality(item);
        item.sellIn--;
        if (isExpired(item)) {
            increaseQuality(item);
        }
    }

    private void updateBackstagePass(Item item) {
        increaseQuality(item);
        if (item.sellIn <= BACKSTAGE_DOUBLE_INCREASE_DAYS) {
            increaseQuality(item);
        }
        if (item.sellIn <= BACKSTAGE_TRIPLE_INCREASE_DAYS) {
            increaseQuality(item);
        }
        item.sellIn--;
        if (isExpired(item)) {
            item.quality = MIN_QUALITY;
        }
    }

    private void updateConjuredItem(Item item) {
        decreaseQualityTwice(item);
        item.sellIn--;
        if (isExpired(item)) {
            decreaseQualityTwice(item);
        }
    }

    private boolean isExpired(Item item) {
        return item.sellIn < 0;
    }

    private void increaseQuality(Item item) {
        if (item.quality < MAX_QUALITY) {
            item.quality++;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > MIN_QUALITY) {
            item.quality--;
        }
    }

    private void decreaseQualityTwice(Item item) {
        decreaseQuality(item);
        decreaseQuality(item);
    }
}

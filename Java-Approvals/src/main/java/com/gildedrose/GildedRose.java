package com.gildedrose;

class GildedRose {
    private final static String AGED_BRIE = "Aged Brie";
    private final static String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    private final static String SULFURAS = "Sulfuras, Hand of Ragnaros";

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            if (item.name.equals(AGED_BRIE)) {
                increaseQualityByOne(item);
            } else if (item.name.equals(BACKSTAGE_PASSES)) {
                increaseQualityBackstage(item);
            } else {
                decreaseQualityByOne(item);
            }

            decreaseSellInEachDay(item);

            if (item.sellIn < 0) {
                if (item.name.equals(AGED_BRIE)) {
                    increaseQualityByOne(item);
                } else if (item.name.equals(BACKSTAGE_PASSES)) {
                    item.quality = 0;
                } else {
                    decreaseQualityByOne(item);
                }
            }
        }
    }

    private void decreaseSellInEachDay(Item item) {
        if (!item.name.equals(SULFURAS)) {
            item.sellIn--;
        }
    }

    private void increaseQualityBackstage(Item item) {
        if (item.sellIn < 6) {
            increaseQualityByThree(item);
        } else if (item.sellIn < 11) {
            increaseQualityByTwo(item);
        } else {
            increaseQualityByOne(item);
        }
    }

    private void decreaseQualityByOne(Item item) {
        if (item.quality > 0) {
            if (!item.name.equals(SULFURAS)) {
                item.quality--;
            }
        }
    }

    private void increaseQualityByOne(Item item) {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    private void increaseQualityByTwo(Item item) {
        increaseQualityByOne(item);
        increaseQualityByOne(item);
    }

    private void increaseQualityByThree(Item item) {
        increaseQualityByTwo(item);
        increaseQualityByOne(item);
    }

}

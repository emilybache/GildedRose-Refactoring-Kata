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
        for (int i = 0; i < items.length; i++) {
            if (items[i].name.equals(AGED_BRIE)) {
                increaseQualityByOne(items[i]);
            } else if (items[i].name.equals(BACKSTAGE_PASSES)) {
                increaseQualityBackstage(i);
            } else {
                decreaseQualityByOne(items[i]);
            }

            decreaseSellInEachDay(items[i]);

            if (items[i].sellIn < 0) {
                if (items[i].name.equals(AGED_BRIE)) {
                    increaseQualityByOne(items[i]);
                } else if (items[i].name.equals(BACKSTAGE_PASSES)) {
                    items[i].quality = 0;
                } else {
                    decreaseQualityByOne(items[i]);
                }
            }
        }
    }

    private void decreaseSellInEachDay(Item item) {
        if (!item.name.equals(SULFURAS)) {
            item.sellIn--;
        }
    }

    private void increaseQualityBackstage(int i) {
        if (items[i].sellIn < 6) {
            increaseQualityByThree(items[i]);
        } else if (items[i].sellIn < 11) {
            increaseQualityByTwo(items[i]);
        } else {
            increaseQualityByOne(items[i]);
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

package com.gildedrose;

import java.util.Arrays;

class GildedRose {
    Item[] items;

    public static String backStagePasses = "Backstage passes to a TAFKAL80ETC concert";
    public static String sulfuras = "Sulfuras, Hand of Ragnaros";
    public static String agedBrie = "Aged Brie";

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Item[] agingItems = Arrays.stream(items).filter(i -> !i.name.equals(sulfuras)).toArray(Item[]::new);
        Item[] bries = Arrays.stream(agingItems).filter(i -> i.name.equals(agedBrie)).toArray(Item[]::new);
        Item[] backstagePasses = Arrays.stream(agingItems).filter(i -> i.name.equals(backStagePasses)).toArray(Item[]::new);
        Item[] otherItems = Arrays.stream(agingItems).filter(i -> !i.name.equals(backStagePasses) && !i.name.equals(agedBrie)).toArray(Item[]::new);
        for (Item item : otherItems) {
            item.sellIn = item.sellIn - 1;
            decreaseQualityOfNormalItems(item);
        }
        for (Item brie : bries) {
            brie.sellIn = brie.sellIn - 1;
            increaseQualityOfBries(brie);
        }
        for (Item item : backstagePasses) {
            item.sellIn = item.sellIn - 1;
            changeQualityOfPasses(item);
        }
    }

    private int getDecreasedQuality(int oldQuality, int factor){
        return Math.max(oldQuality - factor, 0);
    }

    private int getIncreasedQuality(Item item, int factor) {
        return Math.min(item.quality + factor, 50);
    }

    private void decreaseQualityOfNormalItems(Item item) {
        int decreaseFactor = (item.sellIn > 0) ? 1 : 2;
        item.quality = getDecreasedQuality(item.quality, decreaseFactor);
    }

    private void increaseQualityOfBries(Item item) {
        int increaseFactor = (item.sellIn > 0) ? 1 : 2;
        item.quality = getIncreasedQuality(item, increaseFactor);
    }

    private void changeQualityOfPasses(Item item) {
        if (item.sellIn < 0) {
            item.quality = 0;
        } else if (item.sellIn < 5) {
            item.quality = getIncreasedQuality(item, 3);
        } else if (item.sellIn < 10) {
            item.quality = getIncreasedQuality(item, 2);
        } else
            item.quality = getIncreasedQuality(item, 1);
    }
}

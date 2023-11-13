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
            changeQualityOfStandardItems(item);
        }
        for (Item brie : bries) {
            brie.sellIn = brie.sellIn - 1;
            changeQualityOfBries(brie);
        }
        for (Item item : backstagePasses) {
            item.sellIn = item.sellIn - 1;
            changeQualityOfPasses(item);
        }
    }

    private void changeQuality(Item item, int factor) {
        item.quality = Math.min(Math.max(item.quality + factor, 0), 50);
    }
    private void changeQualityOfStandardItems(Item item) {
        final int decreaseFactor = (item.sellIn > 0) ? -1 : -2;
        changeQuality(item, decreaseFactor);
    }
    private void changeQualityOfBries(Item item) {
        final int increaseFactor = (item.sellIn > 0) ? 1 : 2;
        changeQuality(item, increaseFactor);
    }
    private void changeQualityOfPasses(Item item) {
        if (item.sellIn < 0) {
            item.quality = 0;
        } else if (item.sellIn < 5) {
            changeQuality(item, 3);
        } else if (item.sellIn < 10) {
            changeQuality(item, 2);
        } else
            changeQuality(item, 1);
    }
}

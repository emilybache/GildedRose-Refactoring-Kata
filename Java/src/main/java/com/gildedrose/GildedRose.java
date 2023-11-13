package com.gildedrose;

import java.util.Arrays;
import java.util.stream.Stream;

class GildedRose {
    Item[] items;

    public static String agedBrie = "Aged Brie";
    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Item[] agingItems = Arrays.stream(items).filter(i -> !i.name.toLowerCase().contains("sulfuras")).toArray(Item[]::new);
        Stream<Item> bries = Arrays.stream(agingItems).filter(i -> i.name.equals(agedBrie));
        Stream<Item> backstagePasses = Arrays.stream(agingItems).filter(this::isBackstagePass);
        Stream<Item> standardItems = Arrays.stream(agingItems).filter(i -> !isBackstagePass(i) && !i.name.equals(agedBrie));
        for (Item item : agingItems) {
            item.sellIn--;
        }
        bries.forEach(this::changeQualityOfBries);
        backstagePasses.forEach(this::changeQualityOfPasses);
        standardItems.forEach(this::changeQualityOfStandardItems);
    }

    private void changeQuality(Item item, int factor) {
        item.quality = Math.min(Math.max(item.quality + factor, 0), 50);
    }
    private void changeQualityOfStandardItems(Item item) {
        changeQuality(item, (item.sellIn > 0) ? -1 : -2);
    }
    private void changeQualityOfBries(Item item) {
        changeQuality(item, (item.sellIn > 0) ? 1 : 2);
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

    private boolean isBackstagePass(Item i) {
        return i.name.toLowerCase().contains("backstage");
    }

}

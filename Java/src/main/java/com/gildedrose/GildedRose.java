package com.gildedrose;

import java.util.HashMap;
import java.util.Map;

class GildedRose {

    private final static int MIN_QUALITY = 0;
    private final static int MAX_QUALITY = 50;

    private final Item[] items;
    private final Map<String, UpdateQualityStrategy> strategyMap;

    private interface UpdateQualityStrategy {
        void updateQuality(Item item);
    }

    private static class NormalItemUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            if (item.quality > MIN_QUALITY) {
                item.quality = item.quality - 1;
            }
            item.sellIn = item.sellIn - 1;

            if (item.sellIn < 0 && item.quality > MIN_QUALITY) {
                item.quality = item.quality - 1;
            }
        }
    }

    private static class BackstagePassesUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            if (item.quality < MAX_QUALITY) {
                item.quality = item.quality + 1;

                if (item.sellIn < 11 && item.quality < MAX_QUALITY) {
                    item.quality = item.quality + 1;
                }

                if (item.sellIn < 6 && item.quality < MAX_QUALITY) {
                    item.quality = item.quality + 1;
                }
            }

            item.sellIn = item.sellIn - 1;

            if (item.sellIn < MIN_QUALITY) {
                item.quality = MIN_QUALITY;
            }
        }
    }

    private static class AgedBrieUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            if (item.quality < MAX_QUALITY) {
                item.quality = item.quality + 1;
            }

            item.sellIn = item.sellIn - 1;

            if (item.sellIn < MIN_QUALITY && item.quality < MAX_QUALITY) {
                item.quality = item.quality + 1;
            }
        }
    }

    private static class SulfurasUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            // do nothing
        }
    }





    public GildedRose(Item[] items) {
        this.items = items;
        this.strategyMap = new HashMap<String, UpdateQualityStrategy>(){{
            //default value
            put(null, new NormalItemUpdateStrategy());
            // update strategies for each possible key
            put("Backstage passes to a TAFKAL80ETC concert", new BackstagePassesUpdateStrategy());
            put("Aged Brie", new AgedBrieUpdateStrategy());
            put("Sulfuras, Hand of Ragnaros", new SulfurasUpdateStrategy());
        }};
    }

    public void updateQuality() {
        for (Item item : items) {
            UpdateQualityStrategy strategy = strategyMap.getOrDefault(item.name, strategyMap.get(null));
            strategy.updateQuality(item);
        }
    }
}

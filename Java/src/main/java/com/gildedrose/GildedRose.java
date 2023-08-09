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
            changeQuality(item, -1);
            item.sellIn = item.sellIn - 1;

            if (item.sellIn < 0) {
                changeQuality(item, -1);
            }
        }
    }

    private static class BackstagePassesUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            changeQuality(item, 1);
            if (item.sellIn < 11) {
                changeQuality(item, 1);
            }
            if (item.sellIn < 6) {
                changeQuality(item, 1);
            }
            item.sellIn = item.sellIn - 1;
            if (item.sellIn < 0) {
                item.quality = MIN_QUALITY;
            }
        }
    }

    private static class AgedBrieUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            changeQuality(item, 1);

            item.sellIn = item.sellIn - 1;

            if (item.sellIn < 0) {
                changeQuality(item, 1);
            }
        }
    }

    private static class SulfurasUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            // do nothing
        }
    }

    private static class ConjuredUpdateStrategy implements UpdateQualityStrategy {
        @Override
        public void updateQuality(Item item) {
            changeQuality(item, -2);
            item.sellIn = item.sellIn - 1;

            if (item.sellIn < 0) {
                changeQuality(item, -2);
            }
        }
    }

    private static void changeQuality(Item item, int amount) {
        item.quality = amount < 0 ? Math.max(MIN_QUALITY, item.quality + amount) : Math.min(MAX_QUALITY, item.quality + amount);
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
            put("Conjured Mana Cake", new ConjuredUpdateStrategy());
        }};
    }

    public void updateQuality() {
        for (Item item : items) {
            UpdateQualityStrategy strategy = strategyMap.getOrDefault(item.name, strategyMap.get(null));
            strategy.updateQuality(item);
        }
    }
}

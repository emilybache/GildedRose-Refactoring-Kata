package com.gildedrose;

import com.gildedrose.helper.ItemAgentService;

public class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            getUpdater(item).update(item);
        }
    }

    public ItemAgentService getUpdater(Item item) {
        switch (item.name) {
            case "Aged Brie":
                return new AgedBrieUpdater();
            case "Backstage passes to a TAFKAL80ETC concert":
                return new BackstagePassUpdater();
            case "Sulfuras, Hand of Ragnaros":
                return new SulfurasUpdater();
            default:
                return new NormalItemUpdater();
        }
    }

    public static class NormalItemUpdater implements ItemAgentService {
        public void update(Item item) {
            item.sellIn--;
            if (item.quality > 0) item.quality--;
            if (item.sellIn < 0 && item.quality > 0) item.quality--;
        }
    }

    public static class AgedBrieUpdater implements ItemAgentService {
        public void update(Item item) {
            item.sellIn--;
            if (item.quality < 50) item.quality++;
            if (item.sellIn < 0 && item.quality < 50) item.quality++;
        }
    }

    public static class BackstagePassUpdater implements ItemAgentService {
        public void update(Item item) {
            item.sellIn--;
            if (item.sellIn < 0) {
                item.quality = 0;
            } else if (item.sellIn < 5) {
                item.quality = Math.min(item.quality + 3, 50);
            } else if (item.sellIn < 10) {
                item.quality = Math.min(item.quality + 2, 50);
            } else {
                item.quality = Math.min(item.quality + 1, 50);
            }
        }
    }

    public static class SulfurasUpdater implements ItemAgentService {
        public void update(Item item) {
            // Legendary item: no changes
        }
    }

    public static class ConjuredItemUpdater implements ItemAgentService {
        public void update(Item item) {
            item.sellIn--;
            if (item.quality > 0) item.quality -= 2;
            if (item.sellIn < 0 && item.quality > 0) item.quality -= 2;
            if (item.quality < 0) item.quality = 0; // Prevent negative quality
        }
    }


}

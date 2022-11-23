package com.gildedrose;

public class StrorageItem {
    private Item item;

    public StrorageItem(Item item) {
        this.item = item;
    }

    protected static void decreaseQuality(Item item, StrorageItem strorageItem) {
        if (item.quality > 0) {
            item.quality--;
        }
    }

    protected static void increaseQuality(Item item, StrorageItem strorageItem) {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    protected static void updateQuality(Item item, StrorageItem strorageItem) {
        if (item.name.equals("Aged Brie")) {
            increaseQuality(item,strorageItem);
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            increaseQuality(item, strorageItem);

            if (item.sellIn < 11) {
                increaseQuality(item, strorageItem);
            }

            if (item.sellIn < 6) {
                increaseQuality(item, strorageItem);
            }
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else decreaseQuality(item,strorageItem);
    }

    protected static void updateSelling(Item item, StrorageItem strorageItem) {
        if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        }
        item.sellIn--;
    }

    protected static boolean isExpired(Item item, StrorageItem strorageItem) {
        return item.sellIn < 0;
    }

    protected static void updateExpired(Item item, StrorageItem strorageItem) {
        if (item.name.equals("Aged Brie")) {
            increaseQuality(item, strorageItem);
        } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            item.quality = 0;
        } else if (item.name.equals("Sulfuras, Hand of Ragnaros")) {
            return;
        } else {
            decreaseQuality(item, strorageItem);
        }
    }

    public void dailyUpdateItem(Item item) {
        updateQuality(item, this);
        updateSelling(item, this);
        if (isExpired(item, this)) {
            updateExpired(item, this);
        }
    }
}

package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            updateItemQuality(item);
        }
    }

    private void updateItemQuality(Item item) {
        switch (item.name) {
            case "Aged Brie":
                updateQualityForAgedBrie(item);
                break;
            case "Backstage passes to a TAFKAL80ETC concert":
                updateQualityForBackstagePasses(item);
                break;
            case "Sulfuras, Hand of Ragnaros":
                break;
            default:
                updateQualityForRegularItems(item);
                break;
        }

        if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.sellIn--;
        }

        if (item.sellIn < 0) {
            handleExpiredItem(item);
        }
    }

    private void updateQualityForAgedBrie(Item item) {
        if (item.quality < 50) {
            item.quality++;
        }
    }

    private void updateQualityForBackstagePasses(Item item) {
        if (item.quality < 50) {
            item.quality++;
            if (item.sellIn < 11) {
                item.quality = Math.min(item.quality + 1, 50);
            }
            if (item.sellIn < 6) {
                item.quality = Math.min(item.quality + 1, 50);
            }
        }
    }

    private void updateQualityForRegularItems(Item item) {
        if (item.quality > 0) {
            item.quality--;
        }
    }

    private void handleExpiredItem(Item item) {
        switch (item.name) {
            case "Aged Brie":
                if (item.quality < 50) {
                    item.quality++;
                }
                break;
            case "Backstage passes to a TAFKAL80ETC concert":
                item.quality = 0;
                break;
            default:
                if (item.quality > 0) {
                    item.quality--;
                }
                break;
        }
    }
}

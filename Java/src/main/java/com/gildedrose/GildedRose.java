package com.gildedrose;

class GildedRose {

    public static final String AGED_BRIE = "Aged Brie";
    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public static final String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";
    public static final String CONJURED = "Conjured Mana Cake";


    private Item[] items;

    public GildedRose(Item... items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            updateItemQuality(item);
        }
    }

    private void updateItemQuality(Item item) {
        boolean isExpired = item.sellIn < 1;
        int degradeRate = determineDegradeRate(item, isExpired);
        boolean doesDegrade = !item.name.equals(AGED_BRIE) && !item.name.equals(BACKSTAGE) && !item.name.equals(SULFURAS);
        boolean hasSellBy = !item.name.equals(SULFURAS);

        if (doesDegrade) {
            adjustQuality(item, degradeRate);
        }

        if (item.name.equals(AGED_BRIE)) {
            int adjustment = isExpired ? 2 : 1;
            adjustQuality(item, adjustment);
        }

        if (item.name.equals(BACKSTAGE)) {
            updateBackStagePass(item, isExpired);
        }

        if (hasSellBy) {
            item.sellIn = item.sellIn - 1;
        }
    }

    private void updateBackStagePass(Item item, boolean isExpired) {
        adjustQuality(item, 1);
        if (item.sellIn < 11) {
            adjustQuality(item, 1);
        }

        if (item.sellIn < 6) {
            adjustQuality(item, 1);
        }
        if (isExpired) {
            item.quality = item.quality - item.quality;
        }
    }

    private int determineDegradeRate(Item item, boolean isExpired) {
        int baseDegradeRate = item.name.equals(CONJURED) ? -2 : -1;
        return isExpired ? baseDegradeRate * 2 : baseDegradeRate;
    }

    private void adjustQuality(Item item, int adjustment) {
        int newQuality = item.quality + adjustment;
        boolean isInvalidRange = newQuality <= 50 && newQuality >= 0;
        if (isInvalidRange) {
            item.quality = newQuality;
        }
    }

    public Item[] getItems() {
        return items;
    }
}

package com.gildedrose;

/**
 * Class which update quality for all items
 */
class GildedRoseItem {
    Item[] items;
    public static final String SULFURA = "Sulfuras, Hand of Ragnaros";
    public static final String AGED_BRIE = "Aged Brie";
    public static final String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";
    public static final String CONJURED = "Conjured Mana Cake";


    public GildedRoseItem(Item[] items) {
        this.items = items;
    }

    private void updateNumberOfdayToSellRemaining(Item item) {
        item.sellIn -= 1;
    }

    private void updateQualityItem(Item item) {
        if (item.name.equals(AGED_BRIE)) {
            AgedBrie agedBrieItem = new AgedBrie(item);
            agedBrieItem.updateQuaility();
        } else if (item.name.equals(BACKSTAGE)) {
            BackStageItem backStageItem = new BackStageItem(item);
            backStageItem.update();
        } else if (item.name.equals(CONJURED)) {
            ConjuredItem conjuredItem = new ConjuredItem(item);
            conjuredItem.updateQuality();
        } else {
           NormalItem normalItem = new NormalItem(item);
           normalItem.updateQuality();
        }
    }

    public void updateQuality() {
        for (Item item : items) {
            if (item.name.equals(SULFURA)) {continue;}
            updateNumberOfdayToSellRemaining(item);
            updateQualityItem(item);
        }
    }
}

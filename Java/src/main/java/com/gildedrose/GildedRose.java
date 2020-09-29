package com.gildedrose;

class GildedRose {
    private static final int MAX_DEFAULT_QUALITY = 50;
    private static final String BACKSTAGE_ITEM = "Backstage passes to a TAFKAL80ETC concert";
    private static final String CONJURED_ITEM = "Conjured Mana Cake";
    private static final String AGED_ITEM = "Aged Brie";
    private static final String SULFURAS_ITEM = "Sulfuras, Hand of Ragnaros";
    final Item[] items;


    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            reduceSellInItem(i);

            if (isEspecialItem(items[i])) {
                valueItems(i);
            } else {
                devalueItems(i);
            }
            if (items[i].sellIn < 0) {
                qualityWhenDaysAreOver(i);
            }
        }
    }

    private void valueItems(int i) {
        if (canIncreaseQuality(items[i].quality)) {
            if (items[i].name.equals(BACKSTAGE_ITEM)) {
                increaseValueBackstage(i);
            } else {
                increaseQuality(i);
            }
        }
    }

    private void devalueItems(int i) {
        if (items[i].name.equals(CONJURED_ITEM)) {
            reduceQuality(i, 2);
        } else {
            reduceQuality(i, 1);
        }
    }

    private void qualityWhenDaysAreOver(int i) {
        if (items[i].name.equals(AGED_ITEM) && canIncreaseQuality(items[i].quality)) {
            increaseQuality(i);
        } else if (items[i].name.equals(BACKSTAGE_ITEM)) {
            items[i].quality = 0;
        } else {
            if (items[i].quality > 0 && !items[i].name.equals(SULFURAS_ITEM)) {
                reduceQuality(i, 1);
            }
        }
    }

    private void reduceSellInItem(int i) {
        if (!items[i].name.equals(SULFURAS_ITEM)) {
            items[i].sellIn = items[i].sellIn - 1;
        }
    }

    private void increaseValueBackstage(int i) {
        increaseQuality(i);
        if (items[i].sellIn < 11 && canIncreaseQuality(items[i].quality)) {
            increaseQuality(i);
        }
        if (items[i].sellIn < 6 && canIncreaseQuality(items[i].quality)) {
            increaseQuality(i);
        }
    }

    private void increaseQuality(int i) {
        items[i].quality = ++items[i].quality;
    }

    private boolean canIncreaseQuality(int quality) {
        return quality < MAX_DEFAULT_QUALITY;
    }

    private boolean isEspecialItem(Item item) {
        return item.name.equals(AGED_ITEM)
                || item.name.equals(BACKSTAGE_ITEM) || item.name.equals(SULFURAS_ITEM);
    }

    private void reduceQuality(int i, int value) {
        if (items[i].quality > 0) {
            items[i].quality = items[i].quality - value;
        }
    }
}
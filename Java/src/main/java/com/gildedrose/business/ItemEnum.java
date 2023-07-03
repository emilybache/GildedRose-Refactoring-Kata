package com.gildedrose.business;

public enum ItemEnum {
    AGED_BRIE("Aged Brie"),
    SULFURAS("Sulfuras, Hand of Ragnaros"),
    BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
    CONJURED_MANA_CAKE("Conjured Mana Cake");

    private final String value;

    ItemEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}

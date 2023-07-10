package com.gildedrose.item.quality.type;

public enum QualifyUpdaterType {

    AGED_BRIE("Aged Brie"),
    BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
    CONJURED("Conjured"),
    SULFURAS("Sulfuras");


    QualifyUpdaterType(final String name) {
        this.value = name;
    }
    private final String value;

    public String getValue() {
        return value;
    }
}

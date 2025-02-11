package com.gildedrose;

import java.util.Arrays;

public enum ItemType {
    AgedBrie("Aged Brie"),
    BackstagePass("Backstage passes to a TAFKAL80ETC concert"),
    Sulfuras("Sulfuras, Hand of Ragnaros"),
    Conjured("Conjured"),
    Normal("Normal");

    private final String name;

    ItemType(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static ItemType fromName(String name) {
        if (name.startsWith("Conjured")) {
            return Conjured;
        }
        return Arrays.stream(ItemType.values())
            .filter(itemType -> itemType.getName().equals(name))
            .findFirst()
            .orElse(Normal);
    }
}

package com.gildedrose;

import java.util.Arrays;

public enum ItemType {
    AgedBrie("Aged Brie"),
    BackstagePass("Backstage passes to a TAFKAL80ETC concert"),
    Sulfuras("Sulfuras, Hand of Ragnaros");

    private final String name;

    ItemType(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static ItemType fromName(String name) {
        return Arrays.stream(ItemType.values())
            .filter(itemType -> itemType.getName().equals(name))
            .findFirst()
            .orElseThrow( () -> new IllegalArgumentException(name));
    }
}

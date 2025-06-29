package com.gildedrose.domain.item;

import java.util.Arrays;

public enum ItemType {

    STANDARD("Standard item"),
    AGEG_BRIE("Aged Brie"),
    BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
    SULFURAS ("Sulfuras, Hand of Ragnaros")
    ;

    private final String name;

    ItemType(String name) {
        this.name = name;
    }

    public static ItemType findByValue(String itemName) {
        return Arrays.stream(values())
            .filter(itemType -> itemType.getName().equalsIgnoreCase(itemName))
            .findFirst()
            .orElse(STANDARD);
    }

    public String getName(){
        return this.name;
    }
}

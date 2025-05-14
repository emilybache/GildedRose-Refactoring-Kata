package com.gildedrose.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Set;

@Getter
@RequiredArgsConstructor
public enum ItemName {

    BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
    SULFURAS_HAND_RANGAROS("Sulfuras, Hand of Ragnaros"),
    AGED_BRIE("Aged Brie"),
    GENERAL("General item"),
    CONJURED("Conjured Mana Cake");

    private final String name;

    public static ItemName resolve(String name) {
        return Set.of(ItemName.values()).stream()
            .filter(itemName -> name.equalsIgnoreCase(itemName.getName()))
            .findFirst()
            .orElse(GENERAL);
    }

}

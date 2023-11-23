package com.gildedrose;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum Inventory {
    AGED_BRIE("Aged Brie", true, 1),
    LEGENDARY("Sulfuras, Hand of Ragnaros", false, 0),
    CONJURED("Conjured Mana Cake", false, 2),
    BACKSTAGE_PASS("Backstage passes to a TAFKAL80ETC concert", true, 1);

    private String name;
    private boolean qualityDecreaseInverted;
    private int qualityDecrease;
}

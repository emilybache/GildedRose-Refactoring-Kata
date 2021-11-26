package com.gildedrose.item_helpers;

public enum ItemName {

    LEGENDARY("Sulfuras, Hand of Ragnaros"),
    NORMAL("Normal"),
    AGED_BRIE("Aged Brie"),
    BACKSTAGE_PASS("Backstage passes to a TAFKAL80ETC concert"),
    CONJURED("Conjured Mana Cake");

    private final String name;

    ItemName(String input) {
        this.name = input;
    }

    @Override
    public String toString() {
        return name;
    }

    public static ItemName getItemName(String input) {
        for (ItemName itemName : ItemName.values()) {
            if (itemName.name.equalsIgnoreCase(input)) {
                return itemName;
            }
        }
        return NORMAL;
    }
}

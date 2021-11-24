package com.gildedrose.item_helpers;

public enum ItemName {

    SULFURA("Sulfura"),
    NORMAL("Normal"),
    AGED_BRIE("Aged Brie"),
    BACKSTAGE_PASS("Backstage pass"),
    CONJURED("Conjured");

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
            String itemNameStr = itemName.toString().toUpperCase();
            if (itemNameStr.contains(input.toUpperCase()))
                return itemName;
        }
        return NORMAL;
    }
}

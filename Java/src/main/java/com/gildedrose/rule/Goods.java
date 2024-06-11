package com.gildedrose.rule;

public enum Goods {
    AGED_BRIE("Aged Brie"),
    BACKSTAGE("Backstage passes to a TAFKAL80ETC concert"),
    SULFURAS("Sulfuras, Hand of Ragnaros"),
    CONJURED("Conjured");

    public final String label;

    Goods(String label) {
        this.label = label;
    }

    public static Goods valueOfLabel(String label) {
        for (Goods e : values()) {
            if (e.label.equals(label)) {
                return e;
            }
        }
        return null;
    }
}

package com.gildedrose;

import java.util.Arrays;

class GildedRose {
    private static final String BACKSTAGE_PASS_NAME = "Backstage passes to a TAFKAL80ETC concert";
    private static final String SULFURAS_NAME = "Sulfuras, Hand of Ragnaros";
    private static final String AGED_BRIE_NAME = "Aged Brie";
    private static final String CONJURED_NAME = "Conjured";

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        Arrays.stream(items)
            .map(this::mapToUpdatableItem)
            .forEach(UpdatableItem::update);
    }

    private UpdatableItem mapToUpdatableItem(Item item) {
        return switch (item.name) {
            case AGED_BRIE_NAME -> new AgedBrieItem(item);
            case BACKSTAGE_PASS_NAME -> new BackstagePassItem(item);
            case SULFURAS_NAME -> new SulfurasItem(item);
            case CONJURED_NAME -> new ConjuredItem(item);
            default -> new RegularItem(item);
        };
    }
}

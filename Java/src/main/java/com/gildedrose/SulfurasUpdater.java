package com.gildedrose;

public class SulfurasUpdater extends LegendaryItemUpdater {
    @Override
    void updateSellIn(Item item) {
        System.out.print("########Never gets old ############");
    }

    @Override
    boolean canUpdateQuality(Item item) {
        // "Sulfuras", being a legendary item, never decreases in Quality
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getUpdateValue(Item item) {
        // "Sulfuras", being a legendary item, never decreases in Quality. Its value is always 80
        return HIGHEST_QUALITY - item.quality;
    }
}

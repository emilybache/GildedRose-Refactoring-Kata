package com.gildedrose;

class SulfurasUpdater extends LegendaryItemUpdater {

    SulfurasUpdater() {}

    @Override
    void updateSellIn() {
        System.out.print("########Never gets old ############");
    }

    @Override
    boolean canUpdateQuality() {
        // "Sulfuras", being a legendary item, never decreases in Quality
        return item.quality < HIGHEST_QUALITY;
    }

    @Override
    int getUpdateValue() {
        // "Sulfuras", being a legendary item, never decreases in Quality. Its value is always 80
        return HIGHEST_QUALITY - item.quality;
    }
}

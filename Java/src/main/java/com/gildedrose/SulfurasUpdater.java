package com.gildedrose;

public class SulfurasUpdater extends ItemUpdater {
    @Override
    void updateSellIn(Item item) {
        System.out.print("########Never gets old ############");
    }

    @Override
    boolean canUpdateQuality(Item item) {
        // "Sulfuras", being a legendary item, never decreases in Quality
        return false;
    }

    @Override
    int getUpdateValue(Item item) {
        // "Sulfuras", being a legendary item, never decreases in Quality
        return 0;
    }
}

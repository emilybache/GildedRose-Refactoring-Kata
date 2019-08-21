package com.gildedrose;

public class SulfurasUpdater extends ItemUpdater {

    @Override
    void updateQuality(Item item) {
        System.out.println("########Sulfuras is a legendary product");
        // TODO
        item.quality = 80;
    }

    @Override
    void updateSellIn(Item item) {
        System.out.println("########Never gets old");
    }

    @Override
    boolean canUpdateQuality(Item item) {
        // "Sulfuras", being a legendary item, never decreases in Quality
        return false;
    }

    @Override
    int getDegradeValue(Item item) {
        // "Sulfuras", being a legendary item, never has to be sold
        return 0;
    }

    public int getHighestQuality() {
        return 80;
    }
}

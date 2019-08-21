package com.gildedrose;

public class SulfurasUpdater extends ItemUpdater {
    @Override
    void updateQuality(Item item) {
        System.out.println("########Sulfuras is a legendary product");
    }

    @Override
    void updateSellIn(Item item) {
        System.out.println("########Never gets old");
    }
}

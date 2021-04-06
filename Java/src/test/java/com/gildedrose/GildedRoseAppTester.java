package com.gildedrose;

public class GildedRoseAppTester {

    public static GildedRose runFor(int nrOfDays, Item ... items) {
        final GildedRose gildedRose = new GildedRose(items);
        for (int i = 0; i < nrOfDays; i++) {
            gildedRose.updateQuality();
        }
        return gildedRose;
    }
}

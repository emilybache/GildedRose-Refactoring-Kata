package com.gildedrose;

public class GildedRoseAppTester {

    public static GildedRose runFor(int nrOfRuns, Item ... items) {
        final GildedRose gildedRose = new GildedRose(items);
        for (int i = 0; i < nrOfRuns; i++) {
            gildedRose.updateQuality();
        }
        return gildedRose;
    }
}

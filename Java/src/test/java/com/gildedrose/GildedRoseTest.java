package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void givenItem_whenExpiryDateApproaches_thenSellInAndQualityDecreases() {
        int defaultItemSellIn = 10;
        int defaultItemQuality = 20;
        int agedBrieSellIn = 2;
        int agedBrieQuality = 0;
        int backstagePassSellIn = 15;
        int backstagePassQuality = 20;
        GildedRose app = updateQuality(
            createDefaultItem(defaultItemSellIn, defaultItemQuality),
            createAgedBrieItem(agedBrieSellIn, agedBrieQuality),
            createBackstagePass(backstagePassSellIn, backstagePassQuality)
        );

        assertEquals(defaultItemSellIn - 1, app.items[0].sellIn);
        assertEquals(defaultItemQuality - 1, app.items[0].quality);
        assertEquals(agedBrieSellIn - 1, app.items[1].sellIn);
        assertEquals(backstagePassSellIn - 1, app.items[2].sellIn);
    }

    @Test
    void givenItemWithExpiredDate_whenUpdateQuality_thenQualityDecreasesTwiceAsFast() {
        int quality = 5;
        GildedRose app = updateQuality(createDefaultItem(0, 5));

        assertEquals(quality - 2, app.items[0].quality);
    }

    @Test
    void givenItemWithExpiredDate_whenUpdateQuality_thenQualityIsNeverNegative() {
        GildedRose app = updateQuality(createDefaultItem(0, 1));

        assertEquals(0, app.items[0].quality);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
    }

    @Test
    void givenAgedBrieWithWithHighQuality_whenUpdateQuality_thenQualityIsNeverAboveMaximum() {
        GildedRose app = updateQuality(createAgedBrieItem(10, 49));

        assertEquals(50, app.items[0].quality);

        app.updateQuality();

        assertEquals(50, app.items[0].quality);
    }

    @Test
    void givenAgedBrieOrBackstagePass_whenUpdateQuality_thenQualityIncreases() {
        int agedBrieQuality = 12;
        int backstagePassQuality = 8;
        GildedRose app = updateQuality(
            createAgedBrieItem(10, agedBrieQuality),
            createBackstagePass(17, backstagePassQuality)
        );

        assertEquals(agedBrieQuality + 1, app.items[0].quality);
        assertEquals(backstagePassQuality + 1, app.items[1].quality);
    }

    @Test
    void givenBackstagePassLessThan10sellInDays_whenUpdateQuality_thenQualityIncreasesDouble() {
        int quality = 8;
        GildedRose app = updateQuality(createBackstagePass(9, quality));

        assertEquals(quality + 2, app.items[0].quality);
    }

    @Test
    void givenBackstagePassLessThan5sellInDays_whenUpdateQuality_thenQualityIncreasesWithFactor3() {
        int quality = 7;
        GildedRose app = updateQuality(createBackstagePass(3, quality));

        assertEquals(quality + 3, app.items[0].quality);
    }

    @Test
    void givenBackstagePassBeyondSellInDays_whenUpdateQuality_thenQualityIsZero() {
        int quality = 13;
        GildedRose app = updateQuality(createBackstagePass(0, quality));

        assertEquals(0, app.items[0].quality);

        app.updateQuality();

        assertEquals(0, app.items[0].quality);
    }

    @Test
    void givenLegendaryItem_whenUpdateQuality_thenQualityDoesNotChange() {
        int quality = 34;
        GildedRose app = updateQuality(createLegendaryItem(quality));

        assertEquals(quality, app.items[0].quality);
    }

    @Test
    void givenConjuredItem_whenUpdateQuality_thenQualityDecreasesDouble() {
        int quality = 25;
        GildedRose app = updateQuality(createConjuredItem(quality));

        assertEquals(quality - 2, app.items[0].quality);
    }

    private GildedRose updateQuality(Item... items) {
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        return app;
    }

    private Item createDefaultItem(int sellIn, int quality) {
        return new Item("+5 Dexterity Vest", sellIn, quality);
    }

    private Item createAgedBrieItem(int sellIn, int quality) {
        return new Item("Aged Brie", sellIn, quality);
    }

    private Item createBackstagePass(int sellIn, int quality) {
        return new Item("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);
    }

    private Item createLegendaryItem(int quality) {
        return new Item("Sulfuras, Hand of Ragnaros", 10, quality);
    }

    private Item createConjuredItem(int quality) {
        return new Item("Conjured Mana Cake", 6, quality);
    }
}

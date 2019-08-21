package com.gildedrose;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.gildedrose.ItemUpdater.HIGHEST_QUALITY;
import static com.gildedrose.ItemUpdater.MIN_QUALITY;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class GildedRoseTest {

    private final Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);

    private final Item standardWithHighestQuality = new Item("Standard", 5, HIGHEST_QUALITY);
    private final Item agedBrieWithHighestQuality = new Item("Aged Brie", 5, HIGHEST_QUALITY);
    private final Item conjuredWithHighestQuality = new Item("Conjured Mana Cake", 5, HIGHEST_QUALITY);
    private final Item backstageWithHighestQuality = new Item("Backstage passes to a TAFKAL80ETC concert", 5, HIGHEST_QUALITY);

    private final Item standardWithLowQuality = new Item("Standard", 0, MIN_QUALITY + 1);
    private final Item agedBrieWithLowQuality = new Item("Aged Brie", 0, MIN_QUALITY + 1);
    private final Item conjuredWithLowQuality = new Item("Conjured Mana Cake", 0, MIN_QUALITY + 1);
    private final Item backstageWithLowQuality = new Item("Backstage passes to a TAFKAL80ETC concert", 0, MIN_QUALITY + 1);

    private GildedRose app;

    @Test
    public void agedBrie_shouldIncreaseNormal() {
        final int originalQuality = agedBrieWithLowQuality.quality;
        final int originalSellIn = agedBrieWithLowQuality.sellIn;
        final Item[] items = new Item[]{agedBrieWithLowQuality};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals(agedBrieWithLowQuality.name, app.items[0].name);
        assertEquals(originalSellIn - 1, app.items[0].sellIn);
        assertEquals(originalQuality + 1, app.items[0].quality);
    }

    @Test
    public void agedBrie_shouldIncreaseNormal_whenOriginalQualityIsZero() {
        final int originalQuality = 0;
        final int originalSellIn = 2;
        final Item[] items = new Item[]{new Item("Aged Brie", originalSellIn, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        assertEquals(originalSellIn - 1, app.items[0].sellIn);
        assertEquals(originalQuality + 1, app.items[0].quality);
    }

    @Test
    public void agedBrie_shouldNotIncrease_moreThanHighestValue() {
        final int originalQuality = agedBrieWithHighestQuality.quality;
        final int originalSellIn = agedBrieWithHighestQuality.sellIn;
        final Item[] items = new Item[]{agedBrieWithHighestQuality};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals(agedBrieWithHighestQuality.name, app.items[0].name);
        assertEquals(originalSellIn - 1, app.items[0].sellIn);
        assertEquals(originalQuality, app.items[0].quality);
    }

    @Test
    public void backstageItem_shouldIncreaseThriceAsFast_whenLessThan5DaysLeftToSellByDate() {
        final int originalQuality = 20;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(4, app.items[0].sellIn);
        assertEquals(originalQuality + 3, app.items[0].quality);
    }

    @Test
    public void backstageItem_shouldIncreaseTwiceAsFast_whenLessThan10DaysLeftToSellByDate() {
        final int originalQuality = 20;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(9, app.items[0].sellIn);
        assertEquals(originalQuality + 2, app.items[0].quality);
    }

    @Test
    public void backstageItem_shouldIncreaseNormal_whenMoreThan10DaysLeftToSellByDate() {
        final int originalQuality = 20;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(14, app.items[0].sellIn);
        assertEquals(originalQuality + 1, app.items[0].quality);
    }

    @Test
    public void backstageItem_shouldIncreaseNormal_whenQualityIsMinimum() {
        final int originalQuality = MIN_QUALITY;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(14, app.items[0].sellIn);
        assertEquals(originalQuality + 1, app.items[0].quality);
    }

    @Test
    public void backstageItemQuality_shouldBeZero_whenSellByDatePassed() {
        final int originalQuality = 20;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 0, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(-1, app.items[0].sellIn);
        assertEquals(MIN_QUALITY, app.items[0].quality);
    }

    @Test
    public void backstageItemQuality_shouldStayZero_afterTheConcert() {
        final int originalQuality = 20;
        final Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 0, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();

        assertEquals(-1, app.items[0].sellIn);
        assertEquals(MIN_QUALITY, app.items[0].quality);

        app.updateQuality();

        assertEquals(-2, app.items[0].sellIn);
        assertEquals(MIN_QUALITY, app.items[0].quality);
    }

    @Test
    public void conjuredItems_shouldDegradeAsTwiceAsFast_thenTheStandardItems() {
        final Item[] items = new Item[]{conjuredWithHighestQuality};
        app = new GildedRose(items);

        // First 5 times it should degrade by 2
        // Last 2 times (after the sell by date has passed) it should degrade by 4
        for (int i = 0; i < 7; i++) {
            app.updateQuality();
        }

        assertEquals(-2, app.items[0].sellIn);
        assertEquals(32, app.items[0].quality);
    }

    @Test
    public void standardItem_shouldDegradeNormal_whenSellByDateNotPassed() {
        final int originalQuality = 40;
        final Item[] items = new Item[]{new Item("Standard", 4, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Standard", app.items[0].name);
        assertEquals(3, app.items[0].sellIn);
        assertEquals(originalQuality - 1, app.items[0].quality);
    }

    @Test
    public void standardItem_shouldDegradeTwiceAsFast_whenSellByDatePassed() {
        final int originalQuality = 40;
        final Item[] items = new Item[]{new Item("Standard", 0, originalQuality)};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Standard", app.items[0].name);
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(originalQuality - 2, app.items[0].quality);

        app.updateQuality();
        assertEquals(-2, app.items[0].sellIn);
        assertEquals(originalQuality - 4, app.items[0].quality);
    }


    @Test
    public void sulfuras_shouldNeverDegradeAndBeSold() {
        final Item[] items = new Item[]{sulfuras};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        assertEquals(0, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);

        app.updateQuality();
        assertEquals(0, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    public void sulfurasQuality_shouldAlwaysBe80() {
        final Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 0, 40)};
        app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        assertEquals(0, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    public void qualityOfItem_exceptLegendaryItems_cantBeMoreThanHighestValue() {
        final Item[] items = new Item[]{standardWithHighestQuality, backstageWithHighestQuality,
                agedBrieWithHighestQuality, conjuredWithHighestQuality};
        app = new GildedRose(items);
        app.updateQuality();
        final Optional<Item> qualityHigherThan80 = Arrays.stream(app.items).filter(item -> item.quality > HIGHEST_QUALITY).findAny();

        assertFalse(qualityHigherThan80.isPresent());
    }

    @Test
    public void qualityOfItem_onlyLegendaryItems_cantBeMoreThanHighestValue() {
        final Item[] items = new Item[]{sulfuras, standardWithHighestQuality, backstageWithHighestQuality,
                agedBrieWithHighestQuality, conjuredWithHighestQuality};
        app = new GildedRose(items);
        app.updateQuality();
        final List<Item> qualityHigherThan50List = Arrays.stream(app.items).filter(item -> item.quality > HIGHEST_QUALITY).collect(Collectors.toList());

        assertEquals(1, qualityHigherThan50List.size());
        assertEquals(sulfuras.name, qualityHigherThan50List.get(0).name);
    }

    @Test
    public void qualityOfItem_canNeverBeNegative_afterMultipleUpdates() {
        final Item[] items = new Item[]{sulfuras, standardWithLowQuality, backstageWithLowQuality,
                agedBrieWithLowQuality, conjuredWithLowQuality};
        app = new GildedRose(items);

        app.updateQuality();
        final Optional<Item> qualityNegative = Arrays.stream(app.items).filter(item -> item.quality < MIN_QUALITY).findAny();
        assertFalse(qualityNegative.isPresent());

        app.updateQuality();
        final Optional<Item> qualityNegativeAfter2ndUpdate = Arrays.stream(app.items).filter(item -> item.quality < MIN_QUALITY).findAny();
        assertFalse(qualityNegativeAfter2ndUpdate.isPresent());

        final long minimumQualityItemsCount = Arrays.stream(app.items).filter(item -> item.quality == MIN_QUALITY).count();
        // AgedBrie should increase in value, others' quality (3 of them) should be 0
        assertEquals(3, minimumQualityItemsCount);
    }

    @Test
    public void newStandardItem_shouldDegradeAsNormal() {
        ItemUpdaterFactory.registerCustomUpdater("New Item", new StandardItemUpdater());

        final int originalQuality = 40;
        final Item newItem = new Item("New Item", 1, originalQuality);
        final Item[] items = new Item[]{newItem};

        app = new GildedRose(items);
        app.updateQuality();

        assertEquals("New Item", app.items[0].name);
        assertEquals(0, app.items[0].sellIn);
        assertEquals(originalQuality -1, app.items[0].quality);
        assertEquals(newItem.name + ", " + newItem.sellIn + ", " + newItem.quality, app.items[0].toString());
    }
}
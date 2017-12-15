package com.gildedrose;

import static org.junit.Assert.*;

import com.gildedrose.item.Item;
import org.junit.Test;

public class GildedRoseTest {

    @Test
    public void foo() {
        GildedRose app = newGildedRose("foo", 0, 0);

        app.updateQuality();

        assertEquals("foo", app.items[0].name);
    }

    @Test
    public void standardItemDecreasesSellByDayNumberEachTime() {
        GildedRose app = newGildedRose("standard item", 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void brieDecreasesSellByDayNumberEachTime() {
        GildedRose app = newGildedRose("Aged Brie", 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void backstagePassesItemDecreasesSellByDayNumberEachTime() {
        GildedRose app = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void sulfurasItemDoesNotDecreaseSellByDayNumberEachTime() {
        GildedRose app = newGildedRose("Sulfuras, Hand of Ragnaros", 0, 0);

        app.updateQuality();

        assertEquals(0, itemSellByDayNumber(app));
    }

    @Test
    public void brieIncreasesInQualityEachTime() {
        GildedRose app = newGildedRose("Aged Brie", 1, 1);

        app.updateQuality();

        assertEquals(2, itemQualityNumber(app));
    }

    @Test
    public void brieQualityCannotGoAboveFiftyWhenIncreasing() {
        GildedRose app = newGildedRose("Aged Brie", 1, 49);

        app.updateQuality();
        app.updateQuality();

        assertEquals(50, itemQualityNumber(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByOneIfSellByDayMoreThanEleven() {
        GildedRose app = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 12, 1);

        app.updateQuality();

        assertEquals(2, itemQualityNumber(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByTwoIfSellByDayLessThanEleven() {
        GildedRose app = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 10, 1);

        app.updateQuality();

        assertEquals(3, itemQualityNumber(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByThreeIfSellByDayLessThanSix() {
        GildedRose app = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 5, 1);

        app.updateQuality();

        assertEquals(4, itemQualityNumber(app));
    }

    @Test
    public void backstagePassesItemQualityDropsToZeroIfSellByDayHasPassed() {
        GildedRose app = newGildedRose("Backstage passes to a TAFKAL80ETC concert", 0,50);

        app.updateQuality();

        assertEquals(0, itemQualityNumber(app));
    }

    @Test
    public void normalItemDecreasesQualityByOneIfSellByDayIsAboveZero() {
       GildedRose app = newGildedRose("foo", 2, 1);

       app.updateQuality();

       assertEquals(0, itemQualityNumber(app));
    }

    @Test
    public void normalItemDecreasesQualityByTwoOnceSellByDayHasPassed() {
        GildedRose app = newGildedRose("foo",0, 5);

        app.updateQuality();

        assertEquals(3, itemQualityNumber(app));
    }

    @Test
    public void normalItemCannotHaveQualityBelowZero() {
        GildedRose app = newGildedRose("foo", 0, 0);

        app.updateQuality();

        assertEquals(0, itemQualityNumber(app));
    }

    @Test
    public void nothingHappensToSulfurasItem() {
        GildedRose app = newGildedRose("Sulfuras, Hand of Ragnaros", 1, 1);

        app.updateQuality();

        assertEquals(1, itemQualityNumber(app));
        assertEquals(1, itemSellByDayNumber(app));
    }

    private GildedRose newGildedRose(String itemName, int itemSellIn, int itemQuality) {
        Item[] items = new Item[] { new Item(itemName, itemSellIn, itemQuality)};
        return new GildedRose(items);
    }

    private int itemSellByDayNumber(GildedRose app) {
        return app.items[0].sellIn;
    }

    private int itemQualityNumber(GildedRose app) {
        return app.items[0].quality;
    }
}

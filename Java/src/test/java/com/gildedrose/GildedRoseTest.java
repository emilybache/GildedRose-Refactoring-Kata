package com.gildedrose;

import static org.junit.Assert.*;

import com.gildedrose.item.CustomisedItem;
import com.gildedrose.item.Item;
import com.gildedrose.item.CustomisedItemFactory;
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
        GildedRose app = newGildedRose(CustomisedItemFactory.BRIE, 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void backstagePassesItemDecreasesSellByDayNumberEachTime() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void conjuredItemDecreasesSellByDayNumberEachTime() {
        GildedRose app = newGildedRose(CustomisedItemFactory.CONJURED_ITEM, 0, 0);

        app.updateQuality();

        assertEquals(-1, itemSellByDayNumber(app));
    }

    @Test
    public void brieIncreasesInQualityEachTime() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BRIE, 1, 1);

        app.updateQuality();

        assertEquals(2, itemQualityValue(app));
    }

    @Test
    public void brieQualityCannotGoAboveFiftyWhenIncreasing() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BRIE, 1, 49);

        app.updateQuality();
        app.updateQuality();

        assertEquals(50, itemQualityValue(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByOneIfSellByDayMoreThanEleven() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 12, 1);

        app.updateQuality();

        assertEquals(2, itemQualityValue(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByTwoIfSellByDayIsMoreThanSix() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 10, 1);

        app.updateQuality();

        assertEquals(3, itemQualityValue(app));
    }

    @Test
    public void backstagePassesItemDecreasesQualityByThreeIfSellByDayIsMoreThanZero() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 5, 1);

        app.updateQuality();

        assertEquals(4, itemQualityValue(app));
    }

    @Test
    public void backstagePassesItemQualityDropsToZeroIfSellByDayIsZeroOrLess() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 0,50);

        app.updateQuality();

        assertEquals(0, itemQualityValue(app));
    }

    @Test
    public void backstagePassesItemQualityCannotGoAboveFiftyWhenIncreasing() {
        GildedRose app = newGildedRose(CustomisedItemFactory.BACKSTAGE_PASSES_ITEM, 5, 50);

        app.updateQuality();

        assertEquals(50, itemQualityValue(app));
    }

    @Test
    public void standardItemDecreasesQualityByOneIfSellByDayIsAboveZero() {
       GildedRose app = newGildedRose("foo", 2, 1);

       app.updateQuality();

       assertEquals(0, itemQualityValue(app));
    }

    @Test
    public void standardItemDecreasesQualityByTwoOnceSellByDayIsZeroOrLess() {
        GildedRose app = newGildedRose("foo",0, 5);

        app.updateQuality();

        assertEquals(3, itemQualityValue(app));
    }

    @Test
    public void standardItemCannotHaveQualityBelowZero() {
        GildedRose app = newGildedRose("foo", 0, 0);

        app.updateQuality();

        assertEquals(0, itemQualityValue(app));
    }

    @Test
    public void sulfurasHasQualityEighty() {
        GildedRose app = newGildedRose(CustomisedItemFactory.SULFURAS, 1, 80);

        assertEquals(80, itemQualityValue(app));
    }

    @Test
    public void sulfurasItemDoesNotAlterValues() {
        GildedRose app = newGildedRose(CustomisedItemFactory.SULFURAS, 1, 80);

        app.updateQuality();

        assertEquals(80, itemQualityValue(app));
        assertEquals(1, itemSellByDayNumber(app));
    }

    @Test
    public void conjuredItemDecreasesQualityByTwoIfSellByDayIsAboveZero() {
        GildedRose app = newGildedRose(CustomisedItemFactory.CONJURED_ITEM, 2, 5);

        app.updateQuality();

        assertEquals(3, itemQualityValue(app));
    }

    @Test
    public void conjuredItemDecreasesQualityByFourOnceSellByDayIsZeroOrLess() {
        GildedRose app = newGildedRose(CustomisedItemFactory.CONJURED_ITEM,0, 5);

        app.updateQuality();

        assertEquals(1, itemQualityValue(app));
    }

    @Test
    public void conjuredItemCannotHaveQualityBelowZero() {
        GildedRose app = newGildedRose(CustomisedItemFactory.CONJURED_ITEM, 0, 0);

        app.updateQuality();

        assertEquals(0, itemQualityValue(app));
    }

    private GildedRose newGildedRose(String itemName, int itemSellIn, int itemQuality) {
        Item[] items = new Item[] { new Item(itemName, itemSellIn, itemQuality)};
        return new GildedRose(items);
    }

    private int itemSellByDayNumber(GildedRose app) {
        return app.items[0].sellIn;
    }

    private int itemQualityValue(GildedRose app) {
        return app.items[0].quality;
    }
}

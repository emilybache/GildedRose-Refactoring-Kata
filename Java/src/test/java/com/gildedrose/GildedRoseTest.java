package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
  private static final int SULFURAS_QUALITY = 80;

  @Test
  public void agedBrieIncreasesInQualityByOneEachDay() {
    Item[] items = new Item[]{new Item(GildedRose.AGED_BRIE, 2, 0)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 1);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 2);
  }

  @Test
  public void sulfurasSellInDateNeverChanges() {
    int sellIn = 10;
    Item[] items = new Item[]{new Item(GildedRose.SULFURAS, sellIn, SULFURAS_QUALITY)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, sellIn);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, sellIn);
  }

  @Test
  public void sulfurasValueNeverChanges() {
    Item[] items = new Item[]{new Item(GildedRose.SULFURAS, 10, SULFURAS_QUALITY)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, SULFURAS_QUALITY);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, SULFURAS_QUALITY);
  }

  @Test
  public void sulfurasValueIsAlways80() {
    Item[] items = new Item[]{new Item(GildedRose.SULFURAS, 10, SULFURAS_QUALITY)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, SULFURAS_QUALITY);
  }

  @Test
  public void backstagePassesQualityIncreasesByTwoWhenThereAreTenDaysOrLessBeforeTheConcert() {
    int quality = 0;
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 10, quality)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 2);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 4);
  }

  @Test
  public void backstagePassesQualityIncreasesByThreeWhenThereAreFiveDaysOrLessBeforeTheConcert() {
    int quality = 0;
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 5, quality)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 6);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 9);
  }

  @Test
  public void backstagePassesQualityDropsToZeroAfterTheConcert() {
    int quality = 10;
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 1, quality)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 13);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 0);
  }

  @Test
  public void qualityIsNeverNegativeForGenericItems() {
    // todo: should add a similar case for conjured
    int quality = 1;
    Item[] items = new Item[]{new Item("generic item", 5, quality)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 0);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 0);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 0);
  }

  @Test
  public void qualityIsNeverOverFiftyForAgedBrie() {
    Item[] items = new Item[]{new Item(GildedRose.AGED_BRIE, 5, 50)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
  }

  @Test
  public void qualityIsNeverOverFiftyForBackstagePasses() {
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 5, 50)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 50);
  }

  @Test
  public void qualityDecreasesByOneEachDayForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 5, 5)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 4);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 2);
  }

  @Test
  public void qualityDecreasesByTwoEachDayForGenericItemsAfterSellInDate() {
    Item[] items = new Item[]{new Item("generic item", 1, 6)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 5);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 1);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForAgedBrie() {
    Item[] items = new Item[]{new Item(GildedRose.AGED_BRIE, 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 4);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 2);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForBackstagePasses() {
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 4);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 2);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 4);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 3);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 2);
  }

  @Test
  public void sellInDateCanBeNegativeForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 0);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -1);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -2);
  }

  @Test
  public void sellInDateCanBeNegativeForAgedBrie() {
    Item[] items = new Item[]{new Item(GildedRose.AGED_BRIE, 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 0);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -1);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -2);
  }

  @Test
  public void sellInDateCanBeNegativeForBackStagePasses() {
    Item[] items = new Item[]{new Item(GildedRose.BACKSTAGE_PASSES, 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, 0);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -1);
    app.updateQuality();
    assertEquals(app.items.get(0).sellIn, -2);
  }

  @Test
  public void itUpdatesAllItemsPassedIn() {
    Item[] items = new Item[]{
      new Item("generic item 1", 5, 5),
      new Item("generic item 2", 5, 10)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 4);
    assertEquals(app.items.get(1).quality, 9);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 3);
    assertEquals(app.items.get(1).quality, 8);
    app.updateQuality();
    assertEquals(app.items.get(0).quality, 2);
    assertEquals(app.items.get(1).quality, 7);
  }
}

package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
  private static final int SULFURAS_QUALITY = 80;

  @Test
  public void agedBrieIncreasesInQualityByOneEachDay() {
    Item[] items = new Item[]{new Item(ItemType.AGED_BRIE.getDisplayName(), 2, 0)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(1, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(2, app.items.get(0).quality);
  }

  @Test
  public void sulfurasSellInDateNeverChanges() {
    int sellIn = 10;
    Item[] items = new Item[]{
      new Item(ItemType.SULFURAS.getDisplayName(), sellIn, SULFURAS_QUALITY)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(sellIn, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(sellIn, app.items.get(0).sellIn);
  }

  @Test
  public void sulfurasValueNeverChanges() {
    Item[] items = new Item[]{
      new Item(ItemType.SULFURAS.getDisplayName(), 10, SULFURAS_QUALITY)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(SULFURAS_QUALITY, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(SULFURAS_QUALITY, app.items.get(0).quality);
  }

  @Test
  public void sulfurasValueIsAlways80() {
    Item[] items = new Item[]{
      new Item(ItemType.SULFURAS.getDisplayName(), 10, SULFURAS_QUALITY)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(SULFURAS_QUALITY, app.items.get(0).quality);
  }

  @Test
  public void backstagePassesQualityIncreasesByTwoWhenThereAreTenDaysOrLessBeforeTheConcert() {
    int quality = 0;
    Item[] items = new Item[]{
      new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 10, quality)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(2, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(4, app.items.get(0).quality);
  }

  @Test
  public void backstagePassesQualityIncreasesByThreeWhenThereAreFiveDaysOrLessBeforeTheConcert() {
    int quality = 0;
    Item[] items = new Item[]{
      new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 5, quality)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(3, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(6, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(9, app.items.get(0).quality);
  }

  @Test
  public void backstagePassesQualityDropsToZeroAfterTheConcert() {
    int quality = 10;
    Item[] items = new Item[]{
      new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 1, quality)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(13, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(0, app.items.get(0).quality);
  }

  @Test
  public void qualityIsNeverNegativeForGenericItems() {
    // todo: should add a similar case for conjured
    int quality = 1;
    Item[] items = new Item[]{new Item("generic item", 5, quality)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(0, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(0, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(0, app.items.get(0).quality);
  }

  @Test
  public void qualityIsNeverOverFiftyForAgedBrie() {
    Item[] items = new Item[]{new Item(ItemType.AGED_BRIE.getDisplayName(), 5, 50)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
  }

  @Test
  public void qualityIsNeverOverFiftyForBackstagePasses() {
    Item[] items = new Item[]{new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 5, 50)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(50, app.items.get(0).quality);
  }

  @Test
  public void qualityDecreasesByOneEachDayForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 5, 5)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(4, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(3, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(2, app.items.get(0).quality);
  }

  @Test
  public void qualityDecreasesByTwoEachDayForGenericItemsAfterSellInDate() {
    Item[] items = new Item[]{new Item("generic item", 1, 6)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(5, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(3, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(1, app.items.get(0).quality);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForAgedBrie() {
    Item[] items = new Item[]{new Item(ItemType.AGED_BRIE.getDisplayName(), 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(4, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(3, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(2, app.items.get(0).sellIn);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForBackstagePasses() {
    Item[] items = new Item[]{new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(4, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(3, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(2, app.items.get(0).sellIn);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 5, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(4, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(3, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(2, app.items.get(0).sellIn);
  }

  @Test
  public void sellInDateCanBeNegativeForGenericItems() {
    Item[] items = new Item[]{new Item("generic item", 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(0, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-1, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-2, app.items.get(0).sellIn);
  }

  @Test
  public void sellInDateCanBeNegativeForAgedBrie() {
    Item[] items = new Item[]{new Item(ItemType.AGED_BRIE.getDisplayName(), 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(0, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-1, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-2, app.items.get(0).sellIn);
  }

  @Test
  public void sellInDateCanBeNegativeForBackStagePasses() {
    Item[] items = new Item[]{new Item(ItemType.BACKSTAGE_PASSES.getDisplayName(), 1, 1)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(0, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-1, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-2, app.items.get(0).sellIn);
  }

  @Test
  public void itUpdatesAllItemsPassedIn() {
    Item[] items = new Item[]{
      new Item("generic item 1", 5, 5),
      new Item("generic item 2", 5, 10)
    };
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(4, app.items.get(0).quality);
    assertEquals(9, app.items.get(1).quality);
    app.updateQuality();
    assertEquals(3, app.items.get(0).quality);
    assertEquals(8, app.items.get(1).quality);
    app.updateQuality();
    assertEquals(2, app.items.get(0).quality);
    assertEquals(7, app.items.get(1).quality);
  }

  @Test
  public void conjuredItemsDecreaseByTwoEachDayBeforeSellInDate() {
    Item[] items = new Item[]{new Item("Conjured Mana Cake", 10, 10)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(8, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(6, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(4, app.items.get(0).quality);
  }

  @Test
  public void sellInDateDecreasesByOneEachDayForConjuredItem() {
    Item[] items = new Item[]{new Item("Conjured Mana Cake", 2, 10)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(1, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(0, app.items.get(0).sellIn);
    app.updateQuality();
    assertEquals(-1, app.items.get(0).sellIn);
  }

  @Test
  public void conjuredItemsQualityDecreasesByFourEachDayAfterSellInDate() {
    Item[] items = new Item[]{new Item("Conjured Mana Cake", 1, 10)};
    GildedRose app = new GildedRose(items);
    app.updateQuality();
    assertEquals(8, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(4, app.items.get(0).quality);
    app.updateQuality();
    assertEquals(0, app.items.get(0).quality);
  }
}

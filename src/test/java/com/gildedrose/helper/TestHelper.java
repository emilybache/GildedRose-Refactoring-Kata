package com.gildedrose.helper;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemType.OUT_OF_BOUND_QUALITY_MESSAGE;
import static com.gildedrose.item_helpers.ItemType.QUALITY_ERROR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

public class TestHelper {

  public static void testItem(Item item, int daysToPass, int expectedSellIn, int expectedQuality) {
    Item[] items = new Item[1];
    items[0] = item;
    // given
    GildedRose app = new GildedRose(items);
    // when
    for (int i = 0; i < daysToPass; i++) {
      app.updateQuality();
    }
    //then
    assertEquals(expectedSellIn, item.sellIn);
    assertEquals(expectedQuality, item.quality);
  }

  public static void testItemException(Item item) {
    Item[] items = new Item[1];
    items[0] = item;
    GildedRose gildedRose = new GildedRose(items);
    Exception exception = assertThrows(IllegalArgumentException.class, gildedRose::updateQuality);
    String actualMessage = exception.getMessage();
    assertTrue(actualMessage.contains(QUALITY_ERROR_MESSAGE));
  }

  public static void testItemQualityAboveLimitException(Item item) {
    Item[] items = new Item[1];
    items[0] = item;
    GildedRose gildedRose = new GildedRose(items);
    Exception exception = assertThrows(IllegalArgumentException.class, gildedRose::updateQuality);
    String actualMessage = exception.getMessage();
    assertTrue(actualMessage.contains(OUT_OF_BOUND_QUALITY_MESSAGE));
  }

}

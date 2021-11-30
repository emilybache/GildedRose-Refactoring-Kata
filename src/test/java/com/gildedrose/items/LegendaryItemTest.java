package com.gildedrose.items;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.testItem;
import static com.gildedrose.item_helpers.ItemType.NOT_LEGENDARY_ITEM_ERROR_MESSAGE;
import static com.gildedrose.items.LegendaryItem.LEGENDARY;
import static com.gildedrose.items.LegendaryItem.LEGENDARY_ITEM_QUALITY;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(OrderAnnotation.class)
class LegendaryItemTest {

  private final Item item = new Item(LEGENDARY, 5, 80);
  private final Item fakeLegendaryItem = new Item(LEGENDARY, 5, 75);

  @Test
  @Order(1)
  void getLegendaryQualityBeforeSellInDateSuccess() {
    testItem(item, 2, 3, LEGENDARY_ITEM_QUALITY);
  }

  @Test
  @Order(2)
  void getLegendaryQualityPassSellInDateSuccess() {
    testItem(item, 10, -5, LEGENDARY_ITEM_QUALITY);
  }

  @Test
  @Order(3)
  void testFakeLegendaryItemExceptionFail() {
    GildedRose gildedRose = new GildedRose(new Item[]{fakeLegendaryItem});
    Exception exception = assertThrows(IllegalArgumentException.class, gildedRose::updateQuality);
    String actualMessage = exception.getMessage();
    assertTrue(actualMessage.contains(NOT_LEGENDARY_ITEM_ERROR_MESSAGE));
  }

}

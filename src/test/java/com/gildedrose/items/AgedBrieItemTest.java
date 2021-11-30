package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.*;
import static com.gildedrose.items.AgedBrieItem.AGED_BRIE;

@TestMethodOrder(OrderAnnotation.class)
class AgedBrieItemTest {

  private final Item item = new Item(AGED_BRIE, 5, 20);
  private final Item itemError = new Item(AGED_BRIE, 10, -5);
  private final Item itemAboveLimitQuality = new Item(AGED_BRIE, 10, 60);

  @Test
  @Order(1)
  void incrementQualityByOneSuccess() {
    testItem(item, 2, 3, 22);
  }

  @Test
  @Order(2)
  void incrementQualityByTwoSuccess() {
    testItem(item, 7, -2, 29);
  }

  @Test
  @Order(3)
  void negativeQualityFail() {
    testItemException(itemError);
  }

  @Test
  @Order(4)
  void QualityAboveLimitFail() {
    testItemQualityAboveLimitException(itemAboveLimitQuality);
  }

}

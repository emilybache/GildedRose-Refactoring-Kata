package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.*;
import static com.gildedrose.items.BackstagePassItem.BACKSTAGE_PASS;

@TestMethodOrder(OrderAnnotation.class)
class BackstagePassItemTest {

  private final Item item = new Item(BACKSTAGE_PASS, 15, 20);
  private final Item itemError = new Item(BACKSTAGE_PASS, 10, -5);
  private final Item itemAboveLimitQuality = new Item(BACKSTAGE_PASS, 10, 60);

  @Test
  @Order(1)
  void testIncrementQualityByOneSuccess() {
    testItem(item, 5, 10, 25);
  }

  @Test
  @Order(2)
  void testIncrementQualityByTwoSuccess() {
    testItem(item, 10, 5, 35);
  }

  @Test
  @Order(3)
  void testIncrementQualityByThreeSuccess() {
    testItem(item, 15, 0, 50);
  }

  @Test
  @Order(4)
  void testQualityZeroSuccess() {
    testItem(item, 16, -1, 0);
  }

  @Test
  @Order(5)
  void negativeQualityFail() {
    testItemException(itemError);
  }

  @Test
  @Order(6)
  void QualityAboveLimitFail() {
    testItemQualityAboveLimitException(itemAboveLimitQuality);
  }

}

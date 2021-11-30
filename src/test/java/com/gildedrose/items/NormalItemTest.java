package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.*;
import static com.gildedrose.items.NormalItem.NORMAL;

@TestMethodOrder(OrderAnnotation.class)
class NormalItemTest {

  private final Item item = new Item(NORMAL, 5, 20);
  private final Item itemNegativeQuality = new Item(NORMAL, 10, -5);
  private final Item itemAboveLimitQuality = new Item(NORMAL, 10, 60);

  @Test
  @Order(1)
  void decrementQualityByOneSuccess() {
    testItem(item, 2, 3, 18);
  }

  @Test
  @Order(2)
  void decrementQualityByTwoSuccess() {
    testItem(item, 10, -5, 5);
  }

  @Test
  @Order(3)
  void negativeQualityFail() {
    testItemException(itemNegativeQuality);
  }

  @Test
  @Order(4)
  void QualityAboveLimitFail() {
    testItemQualityAboveLimitException(itemAboveLimitQuality);
  }
}

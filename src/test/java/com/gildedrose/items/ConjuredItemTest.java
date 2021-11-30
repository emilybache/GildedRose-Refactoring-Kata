package com.gildedrose.items;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.helper.TestHelper.*;
import static com.gildedrose.items.ConjuredItem.CONJURED;

@TestMethodOrder(OrderAnnotation.class)
class ConjuredItemTest {

  private final Item item = new Item(CONJURED, 5, 20);
  private final Item itemError = new Item(CONJURED, 10, -5);
  private final Item itemAboveLimitQuality = new Item(CONJURED, 10, 60);

  @Test
  @Order(1)
  void decrementQualityByTwoSuccess() {
    testItem(item, 2, 3, 16);
  }

  @Test
  @Order(2)
  void decrementQualityByFourSuccess() {
    testItem(item, 10, -5, 0);
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

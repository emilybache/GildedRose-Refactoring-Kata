package com.gildedrose.main;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.items.AgedBrieItem.AGED_BRIE;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(OrderAnnotation.class)
class ItemClassTest {

  private final Item item = new Item(AGED_BRIE, 5, 20);

  @Test
  @Order(1)
  void testItemSuccess() {
    assertEquals(AGED_BRIE, item.name);
    assertEquals(5, item.sellIn);
    assertEquals(20, item.quality);
    assertEquals("Aged Brie, 5, 20", item.toString());
  }

}

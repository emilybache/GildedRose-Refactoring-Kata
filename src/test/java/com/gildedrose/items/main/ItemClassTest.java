package com.gildedrose.items.main;

import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static com.gildedrose.item_helpers.ItemName.AGED_BRIE;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(OrderAnnotation.class)
class ItemClassTest {

    private final Item item = new Item(AGED_BRIE.toString(), 5, 20);

    @Test
    @Order(1)
    void testItemSuccess() {
        assertEquals(AGED_BRIE.toString(), item.name);
        assertEquals(5, item.sellIn);
        assertEquals(20, item.quality);
        assertEquals("Aged Brie, 5, 20", item.toString());
    }

}

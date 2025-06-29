package com.gildedrose.domain.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class ItemTypeTest {

    @Test
    void findByValue_AgedBrie() {
        // given
        String agedBrie = "Aged Brie";
        //when
        ItemType result = ItemType.findByValue(agedBrie);
        //then
        assertEquals(ItemType.AGEG_BRIE, result);
    }

    @Test
    void findByValue_BackStagePasses() {
        // given
        String agedBrie = "Backstage passes to a TAFKAL80ETC concert";
        //when
        ItemType result = ItemType.findByValue(agedBrie);
        //then
        assertEquals(ItemType.BACKSTAGE_PASSES, result);
    }

    @Test
    void findByValue_Sulfuras() {
        // given
        String agedBrie = "Sulfuras, Hand of Ragnaros";
        //when
        ItemType result = ItemType.findByValue(agedBrie);
        //then
        assertEquals(ItemType.SULFURAS, result);
    }

    @Test
    void findByValue_StandardItem() {
        // given
        String agedBrie = "Elixir of the Mongoose";
        //when
        ItemType result = ItemType.findByValue(agedBrie);
        //then
        assertEquals(ItemType.STANDARD, result);
    }
}

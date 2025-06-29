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
        String backstage = "Backstage passes to a TAFKAL80ETC concert";
        //when
        ItemType result = ItemType.findByValue(backstage);
        //then
        assertEquals(ItemType.BACKSTAGE_PASSES, result);
    }

    @Test
    void findByValue_Sulfuras() {
        // given
        String sulfuras = "Sulfuras, Hand of Ragnaros";
        //when
        ItemType result = ItemType.findByValue(sulfuras);
        //then
        assertEquals(ItemType.SULFURAS, result);
    }

    @Test
    void findByValue_StandardItem() {
        // given
        String standardItem = "Elixir of the Mongoose";
        //when
        ItemType result = ItemType.findByValue(standardItem);
        //then
        assertEquals(ItemType.STANDARD, result);
    }

    @Test
    void findByValue_Conjured() {
        // given
        String conjured = "Conjured Mana Cake";
        //when
        ItemType result = ItemType.findByValue(conjured);
        //then
        assertEquals(ItemType.CONJURED, result);
    }
}

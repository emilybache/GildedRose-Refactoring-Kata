package com.gildedrose.items.multiple_items;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(OrderAnnotation.class)
class MultipleItemsTest {

    public static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
    public static final String LEGENDARY_ITEM_NAME = "Sulfuras, Hand of Ragnaros";

    public static final Item normalItem1 = new Item("+5 Dexterity Vest", 10, 20);
    public static final Item agedBrieItem = new Item("Aged Brie", 2, 0);
    public static final Item normalItem2 = new Item("Elixir of the Mongoose", 5, 7);
    public static final Item legendaryItem1 = new Item(LEGENDARY_ITEM_NAME, 0, 80);
    public static final Item legendaryItem2 = new Item(LEGENDARY_ITEM_NAME, -1, 80);
    public static final Item backStagePassItem1 = new Item(BACKSTAGE_PASS, 15, 20);
    public static final Item backStagePassItem2 = new Item(BACKSTAGE_PASS, 10, 49);
    public static final Item backStagePassItem3 = new Item(BACKSTAGE_PASS, 5, 49);
    public static final Item conjuredItem = new Item("Conjured Mana Cake", 3, 6);

    public static List<Item> getMultipleItems() {
        return asList(normalItem1, agedBrieItem, normalItem2,
            legendaryItem1, legendaryItem2,
            backStagePassItem1, backStagePassItem2, backStagePassItem3,
            conjuredItem);
    }

    @BeforeAll
    static void updateItemsQualityFor20Days() {
        List<Item> items = getMultipleItems();
        GildedRose gildedRose = new GildedRose(items);
        int days = 20;
        for (int i = 0; i < days; i++) {
            gildedRose.updateQuality();
        }
    }

    @Test
    @Order(1)
    void checkNormalItem1() {
        assertEquals(-10, normalItem1.sellIn);
        assertEquals(0, normalItem1.quality);
    }

    @Test
    @Order(2)
    void checkAgedBrie() {
        assertEquals(-18, agedBrieItem.sellIn);
        assertEquals(38, agedBrieItem.quality);
    }

    @Test
    @Order(3)
    void checkNormalItem2() {
        assertEquals(-15, normalItem2.sellIn);
        assertEquals(0, normalItem2.quality);
    }

    @Test
    @Order(4)
    void checkLegendaryItem1() {
        assertEquals(-20, legendaryItem1.sellIn);
        assertEquals(80, legendaryItem1.quality);
    }

    @Test
    @Order(5)
    void checkLegendaryItem2() {
        assertEquals(-21, legendaryItem2.sellIn);
        assertEquals(80, legendaryItem2.quality);
    }

    @Test
    @Order(6)
    void checkBackStagePass1() {
        assertEquals(-5, backStagePassItem1.sellIn);
        assertEquals(0, backStagePassItem1.quality);
    }

    @Test
    @Order(7)
    void checkBackStagePass2() {
        assertEquals(-10, backStagePassItem2.sellIn);
        assertEquals(0, backStagePassItem2.quality);
    }

    @Test
    @Order(8)
    void checkBackStagePass3() {
        assertEquals(-15, backStagePassItem3.sellIn);
        assertEquals(0, backStagePassItem3.quality);
    }

    @Test
    @Order(9)
    void checkConjuredItem() {
        assertEquals(-17, conjuredItem.sellIn);
        assertEquals(0, conjuredItem.quality);
    }

}

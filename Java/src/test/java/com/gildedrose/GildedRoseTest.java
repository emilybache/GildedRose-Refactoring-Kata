package com.gildedrose;

import com.gildedrose.item_helpers.ItemFactory;
import com.gildedrose.item_helpers.ItemType;
import org.junit.jupiter.api.Test;

import static com.gildedrose.item_helpers.ItemName.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void testNormalItem() {
        ItemFactory itemFactory = new ItemFactory();
        int days = 20;
        Item normalItem = new Item(NORMAL.toString(), 10, 20);
        for (int i = 0; i < days; i++) {
            ItemType itemType = itemFactory.getItemType(normalItem);
            itemType.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

    @Test
    void testConjuredItem() {
        ItemFactory itemFactory = new ItemFactory();
        int days = 20;
        Item normalItem = new Item(CONJURED.toString(), 10, 40);
        for (int i = 0; i < days; i++) {
            ItemType itemType = itemFactory.getItemType(normalItem);
            itemType.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }


    @Test
    void testSulfuraItem() {
        ItemFactory itemFactory = new ItemFactory();
        int days = 20;
        Item normalItem = new Item(SULFURA.toString(), 10, 40);
        for (int i = 0; i < days; i++) {
            ItemType itemType = itemFactory.getItemType(normalItem);
            itemType.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

    @Test
    void testAgedBrieItem() {
        ItemFactory itemFactory = new ItemFactory();
        int days = 20;
        Item normalItem = new Item(AGED_BRIE.toString(), 10, 40);
        for (int i = 0; i < days; i++) {
            ItemType itemType = itemFactory.getItemType(normalItem);
            itemType.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

    @Test
    void testBackstagePassItem() {
        ItemFactory itemFactory = new ItemFactory();
        int days = 20;
        Item normalItem = new Item(BACKSTAGE_PASS.toString(), 15, 40);
        for (int i = 0; i < days; i++) {
            ItemType itemType = itemFactory.getItemType(normalItem);
            itemType.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

}

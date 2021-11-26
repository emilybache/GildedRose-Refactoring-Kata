package com.gildedrose;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static com.gildedrose.item_helpers.ItemName.*;

class GildedRoseTest {

    @Test
    void testNormalItem() {
        int days = 20;
        Item normalItem = new Item(NORMAL.toString(), 10, 20);
        GildedRose app = new GildedRose(normalItem);
        app.updateQuality();
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

    @Test
    void testConjuredItem() {
        int days = 20;
        Item normalItem = new Item(CONJURED.toString(), 10, 40);
        GildedRose app = new GildedRose(normalItem);
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(normalItem);
        }
    }

    @Test
    void testLegendaryItem() {
        int days = 20;
        Item legendaryItem = new Item(LEGENDARY.toString(), 10, 80);
        GildedRose app = new GildedRose(legendaryItem);
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(legendaryItem);
        }
    }

    @Test
    void testAgedBrieItem() {
        int days = 20;
        Item agedBrie = new Item(AGED_BRIE.toString(), 10, 40);
        GildedRose app = new GildedRose(agedBrie);
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(agedBrie);
        }
    }

    @Test
    void testBackstagePassItem() {
        int days = 20;
        List<Item> backStagePass = Arrays.asList(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49));
        GildedRose app = new GildedRose(backStagePass);
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            System.out.println("name, sell-in, quality");
            System.out.println(backStagePass);
        }
    }

}

package com.gildedrose;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class GildedRoseTest {
    private final String AGED_BRIE = "Aged Brie";
    private final String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";
    private final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private final String CAKE = "Conjured Mana Cake";
    private final int BACKSTAGE_QUALITY_INCREASE_10_DAYS = 2;
    private final int BACKSTAGE_QUALITY_INCREASE_5_DAYS = 3;
    private GildedRose app;

    @BeforeEach
    void setup() {
        this.app = new GildedRose(new Item[]{});
    }

    @Test
    void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        this.app.items = items;
        this.app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void testNoNegativeQuality() {
        Item[] items = new Item[]{
            new Item(AGED_BRIE, 0, 0),
            new Item(BACKSTAGE, 0, 0),
            new Item(SULFURAS, 0, 0),
            new Item(CAKE, 0, 0)
        };
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        this.app.updateQuality();

        // assert the quality is not negative
        for (Item item :
            this.app.items) {
            assertTrue(item.quality >= 0);
        }
    }

    @Test
    void testAgedBrieQualityIncrease() {
        Item[] items = new Item[]{new Item(AGED_BRIE, 0, 10)};
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        this.app.updateQuality();
        assertTrue(this.app.items[0].quality > 10);
    }


    @Test
    void testQualityNeverMoreThan50() {
        Item[] items = new Item[]{new Item(AGED_BRIE, 0, 50)};
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        this.app.updateQuality();
        assertTrue(this.app.items[0].quality <= 50);
    }


    @Test
    void testSulfurasQualityNeverChange() {
        Item[] items = new Item[]{new Item(SULFURAS, 2, 10)};
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        this.app.updateQuality();
        this.app.updateQuality();
        assertEquals(10,this.app.items[0].quality);
        assertEquals(2,this.app.items[0].sellIn );
    }


    @Test
    void testBackstagePassQuality10DaysBeforeSellIn() {
        Item[] items = new Item[]{new Item(BACKSTAGE, 9, 10)};
        this.app.items = items;
        // update quality two times
        int noOfDays = 3;
        for (int day = 0; day < noOfDays; day++) {
            this.app.updateQuality();
        }
        int expectedQuality = 10 + BACKSTAGE_QUALITY_INCREASE_10_DAYS * noOfDays;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }

    @Test
    void testBackstagePassQuality5DaysBeforeSellIn() {
        Item[] items = new Item[]{new Item(BACKSTAGE, 5, 10)};
        this.app.items = items;
        // update quality two times
        int noOfDays = 3;
        for (int day = 0; day < noOfDays; day++) {
            this.app.updateQuality();
        }
        int expectedQuality = 10 + BACKSTAGE_QUALITY_INCREASE_5_DAYS * noOfDays;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }

    @Test
    void testBackstagePassQuality6DaysBeforeSellIn() {
        Item[] items = new Item[]{new Item(BACKSTAGE, 6, 10)};
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        int expectedQuality = 12;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }

    @Test
    void testBackstagePassQuality11DaysBeforeSellIn() {
        Item[] items = new Item[]{new Item(BACKSTAGE, 11, 10)};
        this.app.items = items;
        // update quality two times
        this.app.updateQuality();
        int expectedQuality = 11;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }

    @Test
    void testBackstagePassQualityAfterSellIn() {
        Item[] items = new Item[]{new Item(BACKSTAGE, 1, 10)};
        this.app.items = items;
        // update quality two times
        int noOfDays = 2;
        for (int day = 0; day < noOfDays; day++) {
            this.app.updateQuality();
        }
        int expectedQuality = 0;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }

    @Test
    void testNormalItemQualityDegradeTwiceAsFast() {
        Item[] items = new Item[]{new Item(CAKE, 0, 10)};
        this.app.items = items;
        // update quality two times
        int noOfDays = 2;
        for (int day = 0; day < noOfDays; day++) {
            this.app.updateQuality();
        }
        int expectedQuality = 6;
        assertEquals(expectedQuality, this.app.items[0].quality);
    }


}

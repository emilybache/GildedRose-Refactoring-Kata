package com.gildedrose;

import org.junit.jupiter.api.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GildedRoseTest {

    private Item[] items;
    private GildedRose app;

    @BeforeEach
    public void init() {
        items = new Item[]{
            new Item("+5 Dexterity Vest", 10, 20),
            new Item("Aged Brie", 2, 0),
            new Item("Elixir of the Mongoose", 5, 7),
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            new Item("Conjured Mana Cake", 3, 6),
            new Item("Conjured Mana Cake", 10, 20)
        };
        app = new GildedRose(items);
    }

    @Nested
    class NormalTests{
        // At the end of each day our system lowers both values (Quality and SellIn) for every item
        @Test
        public void testNormalItemQualityAndSellInDecrease() {
            app.updateQuality();
            assertEquals(19, items[0].quality, "After first day: Normal Quality should decrease by 1");
            assertEquals(9, items[0].sellIn, "After first day: Normal SellIn should decrease by 1");

            app.updateQuality();
            assertEquals(18, items[0].quality, "After second day: Normal Quality should decrease by 2");
            assertEquals(8, items[0].sellIn, "After second day: Normal SellIn should decrease by 2");
        }

        // The Quality of an item is never negative, quality of a normal item is decreasing each day
        @Test
        public void testNormalItemQualityCannotGoNegative() {
            for(int i=0; i<10; i++) {
                app.updateQuality();
            }
            assertTrue(items[2].quality > -1, "Normal Quality should never be negative");
        }

        // Once the sell by date has passed, Quality degrades twice as fast
        @Test
        public void testQualityDegradesTwiceAfterSellPassed() {
            for(int i=0; i<11; i++) {
                app.updateQuality();
            }
            assertEquals(8, items[0].quality, "Normal Quality should degrade twice after sell passed");

            app.updateQuality();
            assertEquals(6, items[0].quality, "Normal Quality should degrade twice after sell passed");
        }
    }


    @Nested
    class AgedBrieTests {
        // The Quality of an item is never more than 50, "Aged Brie" actually increases in Quality the older it gets
        @Test
        public void testAgedBrieItemQualityNeverMoreThan50() {
            for(int i=0; i<100; i++) {
                app.updateQuality();
            }
            assertTrue(items[1].quality <= 50, "Aged Brie Quality should never be more than 50");
        }

        // "Aged Brie" actually increases in Quality the older it gets
        @Test
        public void testAgedBrieItemQualityIncreases() {
            app.updateQuality();
            assertEquals(1, items[1].quality, "Aged Brie Quality should increase");

            app.updateQuality();
            assertEquals(2, items[1].quality, "Aged Brie Quality should increase");
        }

        @Test
        public void testAgedBrieItemSellInDecrease() {
            app.updateQuality();
            assertEquals(1, items[1].sellIn, "Aged Brie SellIn should decrease by 1");

            app.updateQuality();
            assertEquals(0, items[1].sellIn, "Aged Brie SellIn should decrease by 1");

            app.updateQuality();
            assertEquals(-1, items[1].sellIn, "Aged Brie SellIn should decrease by 1");
        }
    }

    @Nested
    class SulfurasTests {
        // "Sulfuras" never has to be sold or decreases in Quality
        @Test
        public void testSulfurasItemQualityUnchanged() {
            app.updateQuality();
            assertEquals(80, items[3].quality, "Sulfuras Quality should be unchanged");

            app.updateQuality();
            assertEquals(80, items[4].quality, "Sulfuras Quality should be unchanged");
        }

        @Test
        public void testSulfurasItemSellInUnchanged() {
            app.updateQuality();
            assertEquals(0, items[3].sellIn, "Sulfuras SellIn should be unchanged");

            app.updateQuality();
            assertEquals(-1, items[4].sellIn, "Sulfuras SellIn should be unchanged");
        }
    }

    @Nested
    class BackstagePassesTests {
        // The Quality of an item is never more than 50, "Backstage passes" increases in Quality
        @Test
        public void testBackstagePassesItemQualityNeverMoreThan50() {
            for(int i=0; i<100; i++) {
                app.updateQuality();
            }
            assertTrue(items[5].quality <= 50, "Backstage Passes Quality should never be more than 50");
        }

        // "Backstage passes" increases in Quality as its SellIn value approaches
        @Test
        public void testBackstagePassesItemQualityIncreases() {
            app.updateQuality();
            assertEquals(21, items[5].quality, "Backstage Passes Quality should increase");

            app.updateQuality();
            assertEquals(50, items[6].quality, "Backstage Passes Quality should increase");
            assertEquals(50, items[7].quality, "Backstage Passes Quality should increase");
        }

        // "Backstage passes" Quality increases by 2 when there are 10 days or less
        @Test
        public void testBackstagePassesItemQualityIncreasesWhenSellInLessThan11() {
            for(int i=0; i<5; i++) {
                app.updateQuality();
            }
            // sellIn is 10, quality is 25

            app.updateQuality();
            assertEquals(27, items[5].quality, "Backstage Passes Quality should increase by 2 when sellIn is 10 or less");

            app.updateQuality();
            assertEquals(29, items[5].quality, "Backstage Passes Quality should increase by 2 when sellIn is 10 or less");
        }

        // "Backstage passes" Quality increases by 3 when there are 5 days or less
        @Test
        public void testBackstagePassesItemQualityIncreasesWhenSellInLessThan6() {
            for(int i=0; i<10; i++) {
                app.updateQuality();
            }
            // sellIn is 5, quality is 35

            app.updateQuality();
            assertEquals(38, items[5].quality, "Backstage Passes Quality should increase by 3 when sellIn is 5 or less");

            app.updateQuality();
            assertEquals(41, items[5].quality, "Backstage Passes Quality should increase by 3 when sellIn is 5 or less");
        }

        // "Backstage passes" Quality drops to 0 after the concert
        @Test
        public void testBackstagePassesItemQualityDropsToZeroAfterConcert() {
            for(int i=0; i<15; i++) {
                app.updateQuality();
            }
            // sellIn is 0, quality is 50

            app.updateQuality();
            assertEquals(0, items[5].quality, "Backstage Passes Quality should drop to zero when sellIn is 0 or less");

            app.updateQuality();
            assertEquals(0, items[5].quality, "Backstage Passes Quality should drop to zero when sellIn is 0 or less");
        }

        @Test
        public void testBackstagePassesItemSellInDecrease() {
            app.updateQuality();
            assertEquals(14, items[5].sellIn, "Backstage Passes SellIn should decrease by 1");
            assertEquals(9, items[6].sellIn, "Backstage Passes SellIn should decrease by 1");

            app.updateQuality();
            assertEquals(3, items[7].sellIn, "Backstage Passes SellIn should decrease by 1");
        }
    }

    @Nested
    class ConjuredTests{
        // The Quality of an item is never negative, "Conjured" items degrade in Quality
        @Test
        public void testConjuredItemQualityCannotGoNegative() {
            for(int i=0; i<10; i++) {
                app.updateQuality();
            }
            assertTrue(items[8].quality > -1, "Conjured Quality should never be negative");
        }

        // "Conjured" items degrade in Quality twice as fast as normal items
        @Test
        public void testConjuredItemQualityDecreasesTwiceAsFast() {
            app.updateQuality();
            assertEquals(4, items[8].quality, "Conjured Quality should decrease twice as fast");

            app.updateQuality();
            assertEquals(16, items[9].quality, "Conjured Quality should decrease twice as fast");
        }

        @Test
        public void testConjuredItemSellInDecrease() {
            app.updateQuality();
            assertEquals(2, items[8].sellIn, "Conjured SellIn should decrease by 1");

            app.updateQuality();
            assertEquals(8, items[9].sellIn, "Conjured SellIn should decrease by 1");
        }
    }

}

package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Set up basic Testing for the initial features and guidelines
 */
class UpdateQualityTest {

    @Test
    void itemDaysLeftDecreasesByONeAtEachIteration() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 5, 7)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Elixir of the Mongoose", app.items[0].name);
        System.out.println("Quality: " + app.items[0].sellIn);
        assertEquals(4, app.items[0].sellIn);
        app.updateQuality();
        assertEquals(3, app.items[0].sellIn);
    }

    @Test
    void itemQualityLeftDecreasesByOneAtEachIteration() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 5, 7)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Elixir of the Mongoose", app.items[0].name);
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(6, app.items[0].quality);
        app.updateQuality();
        assertEquals(5, app.items[0].quality);
    }

    @Test
    void qualityDegradationSpeedAfterOneIterationWhenOutdated() {
        System.out.println("Once the sell by date has passed, Quality degrades twice as fast");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 0, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(18, app.items[0].quality);
        assertEquals("+5 Dexterity Vest", app.items[0].name);
    }

    @Test
    void qualityItemNeverNegatif() {
        System.out.println("The Quality of an item is never negative");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 10, 1)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(0, app.items[0].quality);
        assertEquals("+5 Dexterity Vest", app.items[0].name);
    }

    @Test
    void sellInValueCanBeNegative() {
        System.out.println("SellIn  value of an Item can be negative until quality reach zero");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 0, 30)};
        GildedRose app = new GildedRose(items);
        int timeFrame = 10;
        for (int i = 0; i < timeFrame; i++) {
            app.updateQuality();
        }
        assertEquals(10, app.items[0].quality);
        assertEquals(-timeFrame, app.items[0].sellIn);
    }

    @Test
    void sellInValueCanNotBeNegatifForSulfuras() {
        System.out.println("SellIn  value of Sulfuras Item can not be negative bcse quality never decreases");
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", -1, 80)};
        GildedRose app = new GildedRose(items);
        int timeFrame = 10;
        for (int i = 0; i < timeFrame; i++) {
            app.updateQuality();
        }
        assertEquals(80, app.items[0].quality); // if time is > 1
        assertEquals(-1, app.items[0].sellIn);
    }


    @Test
    void agedBrieQualityIncreaseWthIteration() {
        System.out.println("\"Aged Brie\" actually increases in Quality the older it gets");
        Item[] items = new Item[]{new Item("Aged Brie", 5, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        assertEquals("Aged Brie", app.items[0].name);
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(2, app.items[0].quality);
        app.updateQuality();
        assertEquals(3, app.items[0].quality);
    }

    @Test
    void qualityItemNoMore50() {
        System.out.println("The Quality of an item is never more than 50");
        Item[] items = new Item[]{new Item("Aged Brie", 10, 49)};
        GildedRose app = new GildedRose(items);
        assertEquals("Aged Brie", app.items[0].name);
        app.updateQuality();
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(50, app.items[0].quality);
        app.updateQuality();
        assertEquals(50, app.items[0].quality);

    }

    @Test
    void qualitySulfurasItemNoMore80() {
        System.out.println("The \"Sulfuras\"'s Quality is never more than 80");
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 5, 80)};
        GildedRose app = new GildedRose(items);
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        app.updateQuality();
        System.out.println("Quality: " + app.items[0].quality);
        assertEquals(80, app.items[0].quality);
        app.updateQuality();
        assertEquals(80, app.items[0].quality);
    }


    @Test
    void itemSulfurasNotChangeQuality() {
        System.out.println("\"Sulfuras\", being a legendary item, never decreases in Quality and stays the same");
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 5, 44)};
        GildedRose app = new GildedRose(items);
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        for (int i = 0; i < 10; i++) {
            app.updateQuality();
        }
        assertEquals(44, app.items[0].quality);
    }

    @Test
    void itemBackstageQualityIncreasesByTwoWhenTenDaysOrLess() {
        System.out.println(" Quality increases by 2 when there are 10 days or less");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 9, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 8, 0)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        for (int i = 0; i < 2; i++) {
            assertEquals(2, app.items[i].quality);
        }
    }

    @Test
    void itemBackstageQualityIncreasesByThreeWhenFiveDaysOrLess() {
        System.out.println("Quality increases by 3 when there are 5 days or less");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 4, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 2, 3)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        for (int i = 0; i < 2; i++) {
            assertEquals(6, app.items[i].quality);
        }

    }

    @Test
    void itemBackstageQualityDropsToZeroAfterTheConcert() {
        System.out.println("Quality drops to 0 after the concert");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 2),
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 7)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        for (Item el : items) {
            assertEquals(0, el.quality);
        }
    }

    /**
     * Testing for "Conjured" items
     */


//    @Test
//    void itemConjuredQualityTwiceAsFastAsNormalItems() {
//        System.out.println("\"Conjured\" items degrade in Quality twice as fast as normal items");
//        Item[] items = new Item[]{
//            new Item("Conjured Mana Cake", 3, 6)};
//        GildedRose app = new GildedRose(items);
//        app.updateQuality();
//        assertEquals(4, app.items[0].quality);
//    }

}



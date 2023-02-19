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
        assertEquals(4, app.items[0].getSellIn());
        app.updateQuality();
        assertEquals(3, app.items[0].getSellIn());
    }

    @Test
    void itemQualityLeftDecreasesByOneAtEachIteration() {
        Item[] items = new Item[]{new Item("Elixir of the Mongoose", 5, 7)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(6, app.items[0].getQuality());
        app.updateQuality();
        assertEquals(5, app.items[0].getQuality());
    }

    @Test
    void qualityDegradationSpeedAfterOneIterationWhenZERO() {
        System.out.println("Once the sell by date is zero, Quality degrades twice as fast");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 0, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(18, app.items[0].getQuality());
        assertEquals("+5 Dexterity Vest", app.items[0].getName());
    }

    @Test
    void qualityDegradationSpeedAfterOneIterationWhenOutdated() {
        System.out.println("Once the sell by date has passed, Quality degrades twice as fast");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", -1, 18)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(16, app.items[0].getQuality());
        assertEquals("+5 Dexterity Vest", app.items[0].getName());
    }

    @Test
    void qualityItemNeverNegative() {
        System.out.println("The Quality of an item is never negative");
        Item[] items = new Item[]{
            new Item("+5 Dexterity Vest", 10, 1),
            new Item("Conjured Mana Cake", 10, 1)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        assertEquals(0, app.items[0].getQuality());
        assertEquals(0, app.items[1].getQuality());
    }

    @Test
    void sellInValueIsOneForNormalItem() {
        System.out.println("Normal item SellIn value is one to test if statement limit for negative value");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 1, 30)};
        GildedRose app = new GildedRose(items);
            app.updateQuality();
        assertEquals(29, app.items[0].getQuality());
        assertEquals(0, app.items[0].getSellIn());
    }

    @Test
    void sellInValueIsOneForAgedBrie() {
        System.out.println("Aged Brie item SellIn value is one to test if statement limit for negative value");
        Item[] items = new Item[]{new Item("Aged Brie", 1, 30)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(31, app.items[0].getQuality());
        assertEquals(0, app.items[0].getSellIn());
    }
    @Test
    void sellInValueCanBeNegativeStartAtZero() {
        System.out.println("SellIn  value of an Item can be negative until quality reach zero: start with a zero sellin Value");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", 0, 30)};
        GildedRose app = new GildedRose(items);
        int timeFrame = 10;
        for (int i = 0; i < timeFrame; i++) {
            app.updateQuality();
        }
        assertEquals(10, app.items[0].getQuality());
        assertEquals(-timeFrame, app.items[0].getSellIn());
    }

    @Test
    void sellInValueCanBeNegativeStartNegativeSellInValue() {
        System.out.println("SellIn  value of an Item can be  until quality reach zero: start with a negative sellinValue");
        Item[] items = new Item[]{new Item("+5 Dexterity Vest", -5, 30)};
        GildedRose app = new GildedRose(items);
        int timeFrame = 10;
        for (int i = 0; i < timeFrame; i++) {
            app.updateQuality();
        }
        assertEquals(10, app.items[0].getQuality());
        assertEquals(-5 -timeFrame, app.items[0].getSellIn());
    }

    @Test
    void sellInValueCanNotChangeForSulfuras() {
        System.out.println("SellIn value of Sulfuras Item can not change");
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", -1, 80)};
        GildedRose app = new GildedRose(items);
        for (int i = 0; i < 10; i++) {
            app.updateQuality();
        }
        assertEquals(-1, app.items[0].getSellIn());
    }


    @Test
    void agedBrieQualityIncreaseWthIteration() {
        System.out.println("\"Aged Brie\" actually increases in Quality the older it gets");
        Item[] items = new Item[]{new Item("Aged Brie", 5, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        app.updateQuality();
        assertEquals(2, app.items[0].getQuality());
        app.updateQuality();
        assertEquals(3, app.items[0].getQuality());
    }

    @Test
    void agedBrieQualityIncreaseWthIterationFasterWhenOutdated() {
        System.out.println("\"Aged Brie\" actually increases in Quality the older it gets and twicer when outdatd");
        Item[] items = new Item[]{new Item("Aged Brie", -1, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(22, app.items[0].getQuality());
    }


    @Test
    void qualityItemNoMore50ForAgedBrie() {
        System.out.println("The Quality of Aged Brie item is never more than 50");
        Item[] items = new Item[]{new Item("Aged Brie", 10, 49)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].getQuality());
        app.updateQuality();
        assertEquals(50, app.items[0].getQuality());
    }

    @Test
    void qualityItemNoMore50ForBackstage() {
        System.out.println("The Quality of Backstage passes to a TAFKAL80ETC concert item is never more than 50");
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].getQuality());
        app.updateQuality();
        assertEquals(50, app.items[0].getQuality());
    }

    @Test
    void itemSulfurasNotChangeQuality() {
        System.out.println("\"Sulfuras\", being a legendary item, never decreases in Quality and stays the same");
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 5, 55)};
        GildedRose app = new GildedRose(items);
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].getName());
        for (int i = 0; i < 10; i++) {
            app.updateQuality();
        }
        assertEquals(55, app.items[0].getQuality());
    }

    @Test
    void itemBackstageQualityIncreasesByTwoWhenTenDaysOrLess() {
        System.out.println(" Quality increases by 2 when there are 10 days or less");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 9, 0),
            new Item("Backstage passes to a TAFKAL80ETC concert", 8, 48)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(2, app.items[0].getQuality());
        assertEquals(2, app.items[1].getQuality());
        assertEquals(50, app.items[2].getQuality());

    }

    @Test
    void itemBackstageQualityIncreasesByThreeWhenFiveDaysOrLess() {
        System.out.println("Quality increases by 3 when there are 5 days or less");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 4, 3),
            new Item("Backstage passes to a TAFKAL80ETC concert", 3, 48)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(6, app.items[0].getQuality());
        assertEquals(6, app.items[1].getQuality());
        assertEquals(50, app.items[2].getQuality());

    }

    @Test
    void itemBackstageQualityDropsToZeroAfterTheConcert() {
        System.out.println("Quality drops to 0 after the concert");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 2),
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 11),
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 48)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].getQuality());
        assertEquals(0, app.items[1].getQuality());
        assertEquals(0, app.items[2].getQuality());

    }

    @Test
    void itemBackstageQualityDropsToZeroAfterTheConcertEvenIf50() {
        System.out.println("Quality drops to 0 after the concert even if quality is 50");
        Item[] items = new Item[]{
            new Item("Backstage passes to a TAFKAL80ETC concert", 0, 50),
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].getQuality());
    }

    /**
     * Testing for "Conjured" items
     */


    @Test
    void itemConjuredQualityTwiceAsFastAsNormalItemsWithPositiveSellIn() {
        System.out.println("\"Conjured\" items degrade in Quality twice as fast as normal items when sellin is positive");
        Item[] items = new Item[]{
            new Item("Conjured Mana Cake", 3, 6)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(4, app.items[0].getQuality());
    }

    @Test
    void itemConjuredQualityTwiceAsFastAsNormalItemsWhenSellInIsZero() {
        System.out.println("\"Conjured\" items degrade in Quality twice as fast as normal items when sellin is wero");
        Item[] items = new Item[]{
            new Item("Conjured Mana Cake", 0, 10)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(6, app.items[0].getQuality());
    }

    @Test
    void itemConjuredQualityTwiceAsFastAsNormalItemsWithNegativeSellIn() {
        System.out.println("\"Conjured\" items degrade in Quality twice as fast as normal items when sellin is negtive");
        Item[] items = new Item[]{
            new Item("Conjured Mana Cake", -1, 8)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(4, app.items[0].getQuality());
    }
}



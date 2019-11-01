package com.gildedrose;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class GildedRoseTest {

    @Test
    public void foo() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    @DisplayName("At the end of each day our system lowers both values for every item")
    public void shouldLowerBothValues() {
        Item[] items = new Item[]{TestHelper.getItem("foobar", 2, 2)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(1, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);
    }

    @Test
    @DisplayName("Once the sell by date has passed (<=0), Quality degrades twice as fast")
    public void shouldDowngradeTwiceAsFastAfterSellDate() {
        Item[] items = new Item[]{TestHelper.getItem("foobar", 1, 5)};
        GildedRose app = new GildedRose(items);

        //day 1, drop by 1
        app.updateQuality();
        assertEquals(4, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        //day 2, drop by 2
        app.updateQuality();
        assertEquals(2, app.items[0].quality);
        assertEquals(-1, app.items[0].sellIn);
    }

    @Test
    @DisplayName("The Quality of an item is never negative")
    public void shouldNeverHaveANegativeQuality() {
        Item[] items = new Item[]{TestHelper.getItem("foobar", 0, 0)};
        GildedRose app = new GildedRose(items);

        //day 1, drop by 1
        app.updateQuality();
        assertEquals(0, app.items[0].quality);

        //day 2, drop by 1 => quality is still 0
        app.updateQuality();
        assertEquals(0, app.items[0].quality);
    }


    @Test
    @DisplayName("\"Aged Brie\" actually increases in Quality by 2 the older it gets")
    public void shouldSeeAgedBrieIncreasedQualityDayByDay() {
        Item[] items = new Item[]{TestHelper.getItem("Aged Brie", 0, 0)};
        GildedRose app = new GildedRose(items);

        //day 1, add 2
        app.updateQuality();
        assertEquals(2, app.items[0].quality);

        //day 2, add 2
        app.updateQuality();
        assertEquals(4, app.items[0].quality);
    }

    @Test
    @DisplayName("The Quality of an item is never more than 50")
    public void shouldNeverHaveQualityHigherThan50() {
        Item[] items = new Item[]{TestHelper.getItem("Aged Brie", 0, 49)};
        GildedRose app = new GildedRose(items);

        //day 1, add 2
        app.updateQuality();
        assertEquals(50, app.items[0].quality);

        //day 2, add 2
        app.updateQuality();
        assertEquals(50, app.items[0].quality);
    }

    //Sulfuras, Hand of Ragnaros
    @Test
    @DisplayName("\"Sulfuras\", being a legendary item, never has to be sold or decreases in Quality")
    public void shouldNotChangeSulfurasSellInAndQualityValues() {
        Item[] items = new Item[]{TestHelper.getItem("Sulfuras, Hand of Ragnaros", 0, 10)};
        GildedRose app = new GildedRose(items);

        //day 1, don't impact quality
        app.updateQuality();
        assertEquals(10, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);

        //day 0, don't impact quality
        app.updateQuality();
        assertEquals(10, app.items[0].quality);
        assertEquals(0, app.items[0].sellIn);
    }

    @Test
    @DisplayName("\"Backstage passes\", like aged brie, increases in Quality as its SellIn value approaches")
    public void shouldIncreaseQualityForBrieWhenSellInReduces() {
        Item[] items = new Item[]{TestHelper.getItem("Backstage passes to a TAFKAL80ETC concert", 5, 10)};
        GildedRose app = new GildedRose(items);

        //day 1, increase by 3 the quality
        app.updateQuality();
        assertEquals(13, app.items[0].quality);
        assertEquals(4, app.items[0].sellIn);

        //day 1, increase by 3 the quality
        app.updateQuality();
        assertEquals(16, app.items[0].quality);
        assertEquals(3, app.items[0].sellIn);
    }

    @Nested
    @DisplayName("Backstage passes")
    class BackstagePasses{
        @Test
        @DisplayName("Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but")
        public void shouldIncreaseQualityBasedOnSellIn() {
            Item[] items = new Item[]{
                    TestHelper.getItem("Backstage passes to a TAFKAL80ETC concert", 10, 10),
                    TestHelper.getItem("Backstage passes to a TAFKAL80ETC concert", 5, 10)
            };
            GildedRose app = new GildedRose(items);

            app.updateQuality();
            assertEquals(12, app.items[0].quality);
            assertEquals(9, app.items[0].sellIn);

            assertEquals(13, app.items[1].quality);
            assertEquals(4, app.items[1].sellIn);
        }

        @Test
        @DisplayName("Quality drops to 0 after the concert")
        public void shouldHaveQualityDroppingToZeroAfterConcert(){
            Item[] items = new Item[]{
                    TestHelper.getItem("Backstage passes to a TAFKAL80ETC concert", 0, 10)
            };

            GildedRose app = new GildedRose(items);

            app.updateQuality();
            assertEquals(0, app.items[0].quality);
            assertEquals(-1, app.items[0].sellIn);
        }
    }

    @Nested
    @DisplayName("Quality limits for")
    class QualityBound{

        @Test
        @DisplayName("an item can never have its Quality increase above 50")
        public void shouldNeverHaveQualityAbove50(){
            Item[] items = new Item[]{
                    TestHelper.getItem("Backstage passes to a TAFKAL80ETC concert", 10, 49)
            };

            GildedRose app = new GildedRose(items);

            app.updateQuality();
            assertEquals(50, app.items[0].quality);
            assertEquals(9, app.items[0].sellIn);
        }

        @Test
        @DisplayName("however \"Sulfuras\" is a legendary item and as such its Quality is 80 and it never alters")
        public void shouldSulfurasNeverAltersForSulfuras(){
            Item[] items = new Item[]{
                    TestHelper.getItem("Sulfuras, Hand of Ragnaros", 10, 80)
            };

            GildedRose app = new GildedRose(items);

            app.updateQuality();
            assertEquals(80, app.items[0].quality);
        }
    }

}
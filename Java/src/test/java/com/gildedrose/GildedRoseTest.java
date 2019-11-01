package com.gildedrose;

import static org.junit.Assert.*;

import org.junit.Test;

public class GildedRoseTest {

    @Test
    public void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    //At the end of each day our system lowers both values for every item
    @Test
    public void shouldLowerBothValues(){
        Item[] items = new Item[] { TestHelper.getItem("foobar", 2, 2) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(1, app.items[0].quality);
        assertEquals(1, app.items[0].sellIn);
    }

    //Once the sell by date has passed (<=0), Quality degrades twice as fast
    @Test
    public void shouldDowngradeTwiceAsFastAfterSellDate(){
        Item[] items = new Item[] { TestHelper.getItem("foobar", 1, 5) };
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

}

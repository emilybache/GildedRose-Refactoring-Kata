package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    public void agedBrieQualityIncreasesOverTime() {
        Item agedBrie = new Item("Aged Brie", 10, 10);
        GildedRose gildedRose = new GildedRose(new Item[]{agedBrie});
        gildedRose.updateQuality();
        assertEquals(11, agedBrie.quality);
    }

    @Test
    public void backstagePassesQualityIncreasesOverTimeThenZeroesOut() {
        Item backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10);
        GildedRose gildedRose = new GildedRose(new Item[]{backstagePasses});
        gildedRose.updateQuality();
        assertEquals(13, backstagePasses.quality);

        gildedRose.updateQuality();
        assertEquals(16, backstagePasses.quality);

        gildedRose.updateQuality();
        assertEquals(19, backstagePasses.quality);

        gildedRose.updateQuality();
        assertEquals(22, backstagePasses.quality);
    }

    @Test
    public void normalItemQualityDecreasesOverTime() {
        Item normalItem = new Item("Conjured Mana Cake", 10, 10);
        GildedRose gildedRose = new GildedRose(new Item[]{normalItem});
        gildedRose.updateQuality();
        assertEquals(9, normalItem.quality);
    }

    @Test
    public void testUpdateQuality() {
        Item[] items = new Item[]{
            new Item("+5 Dexterity Vest", 10, 20),
            new Item("Aged Brie", 2, 0),
            new Item("Elixir of the Mongoose", 5, 7),
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Conjured Mana Cake", 3, 6)
        };

        GildedRose gildedRose = new GildedRose(items);
        gildedRose.updateQuality();
        assertEquals(9, gildedRose.items[0].sellIn);
        assertEquals(19, gildedRose.items[0].quality);


        assertEquals(1, gildedRose.items[1].sellIn);
        assertEquals(2, gildedRose.items[1].quality);

        assertEquals(4, gildedRose.items[2].sellIn);
        assertEquals(6, gildedRose.items[2].quality);

        assertEquals(0, gildedRose.items[3].sellIn);
        assertEquals(80, gildedRose.items[3].quality);

        assertEquals(14, gildedRose.items[4].sellIn);
        assertEquals(21, gildedRose.items[4].quality);

        assertEquals(2, gildedRose.items[5].sellIn);
        assertEquals(5, gildedRose.items[5].quality);
    }


}

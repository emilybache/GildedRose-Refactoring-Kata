package com.gildedrose;

import com.gildedrose.item.Item;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SulfurasTest {

    @Test
    void sulfurasNeverChanges() {
        //given
        Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        Item[] items = new Item[] {sulfuras};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(0, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    void sulfurasQualityLevelIsAlways80() {
        //given
        Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        Item[] items = new Item[] {sulfuras};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(80, app.items[0].quality);
    }

}

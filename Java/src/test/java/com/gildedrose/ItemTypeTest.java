package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.ItemType.AgedBrie;
import static com.gildedrose.ItemType.BackstagePass;
import static com.gildedrose.ItemType.Normal;
import static com.gildedrose.ItemType.Sulfuras;
import static com.gildedrose.ItemType.fromName;
import static org.junit.jupiter.api.Assertions.*;

class ItemTypeTest {

    @Test
    void testGetName() {
        assertEquals("Aged Brie", AgedBrie.getName());
        assertEquals("Backstage passes to a TAFKAL80ETC concert", BackstagePass.getName());
        assertEquals("Sulfuras, Hand of Ragnaros", Sulfuras.getName());
        assertEquals("Normal", Normal.getName());
    }

    @Test
    void testFromName_ValidNames() {
        assertEquals(AgedBrie, fromName("Aged Brie"));
        assertEquals(BackstagePass, fromName("Backstage passes to a TAFKAL80ETC concert"));
        assertEquals(Sulfuras, fromName("Sulfuras, Hand of Ragnaros"));
        assertEquals(Normal, fromName("Normal"));
    }

    @Test
    void testFromName_InvalidName() {
        assertEquals(Normal, fromName("Nonexistent Item"));
    }

    @Test
    void testFromName_NullInput() {
        assertEquals(Normal, fromName(null));
    }
}

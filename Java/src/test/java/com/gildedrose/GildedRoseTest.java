package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class GildedRoseTest {

    private Item createItem(String name, int sellIn, int quality) {
        return new Item(name, sellIn, quality);
    }

    @Test
    void normaleItem_QualitaetSinktProTag() {
        Item item = createItem("Normaler Gegenstand", 10, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(19, item.quality); // -1 Qualität
    }

    @Test
    void normaleItem_QualitaetSinktDoppeltNachVerfall() {
        Item item = createItem("Normaler Gegenstand", 0, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(18, item.quality); // -2 Qualität
    }

    @Test
    void agedBrie_QualitaetSteigt() {
        Item item = createItem("Aged Brie", 10, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(21, item.quality); // +1 Qualität
    }

    @Test
    void agedBrie_QualitaetSteigtSchnellerNachVerfall() {
        Item item = createItem("Aged Brie", -1, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(22, item.quality); // +2 Qualität
    }

    @Test
    void backstagePass_QualitaetSteigtUm3Bei5Tagen() {
        Item item = createItem("Backstage passes to a TAFKAL80ETC concert", 5, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(23, item.quality); // +3 Qualität
    }

    @Test
    void backstagePass_QualitaetFälltAuf0NachKonzert() {
        Item item = createItem("Backstage passes to a TAFKAL80ETC concert", 0, 20);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.quality); // Qualität = 0
    }

    @Test
    void sulfuras_QualitaetUnverändert() {
        Item item = createItem("Sulfuras, Hand of Ragnaros", 10, 80);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(80, item.quality); // Qualität bleibt 80
    }

    @Test
    void qualitaetNieÜber50() {
        Item item = createItem("Aged Brie", 10, 50);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(50, item.quality); // Maximalwert 50
    }

    @Test
    void qualitaetNieUnter0() {
        Item item = createItem("Normaler Gegenstand", 10, 0);
        GildedRose app = new GildedRose(new Item[]{item});
        app.updateQuality();
        assertEquals(0, item.quality); // Minimalwert 0
    }
}

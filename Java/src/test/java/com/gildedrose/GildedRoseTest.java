package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    /**
     * Simple quality degradation
     */
    @Test
    void whenSale1Quality2_afterUpdate_ThenSale0Quality1() {
        Item[] items = new Item[]{new Item("foo", 1, 2)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(0, item.sellIn);
        assertEquals(1, item.quality);
    }

    /**
     * Simple quality degradation
     */
    @Test
    void whenSale1Quality1_afterUpdate_ThenSale0Quality0() {
        Item[] items = new Item[]{new Item("foo", 1, 1)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(0, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Quality never lower than zero
     */
    @Test
    void whenSale0Quality0_afterUpdate_ThenSaleM1Quality0() {
        Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Double quality degradation
     */
    @Test
    void whenSaleM10Quality8_afterUpdate_ThenSaleM11Quality6() {
        Item[] items = new Item[]{new Item("foo", -10, 8)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(-11, item.sellIn);
        assertEquals(6, item.quality);
    }

    /**
     * Double quality degradation edge
     */
    @Test
    void whenSale0Quality10_afterUpdate_ThenSaleM1Quality8() {
        Item[] items = new Item[]{new Item("foo", 0, 10)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(8, item.quality);
    }

    /**
     * Double quality degradation edge
     */
    @Test
    void whenSale0Quality2_afterUpdate_ThenSaleM1Quality0() {
        Item[] items = new Item[]{new Item("foo", 0, 2)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("foo", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Conjured Quality decreases 2 before expired
     */
    @Test
    void whenConjuredSale1Quality2_afterUpdate_ThenSale0Quality0() {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", 1, 2)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Conjured Mana Cake", item.name);
        assertEquals(0, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Conjured Quality decreases 4 after expired
     */
    @Test
    void whenConjuredSaleM10Quality8_afterUpdate_ThenSaleM11Quality4() {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", -10, 8)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Conjured Mana Cake", item.name);
        assertEquals(-11, item.sellIn);
        assertEquals(4, item.quality);
    }

    /**
     * Conjured Quality decreases 4 after expired edge
     */
    @Test
    void whenConjuredSale0Quality10_afterUpdate_ThenSaleM1Quality6() {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", 0, 10)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Conjured Mana Cake", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(6, item.quality);
    }

    /**
     * Conjured Quality never lower than 0
     */
    @Test
    void whenConjuredSale0Quality0_afterUpdate_ThenSaleM1Quality0() {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", 0, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Conjured Mana Cake", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Conjured Quality never lower than 0
     */
    @Test
    void whenConjuredSale1Quality1_afterUpdate_ThenSale0Quality0() {
        Item[] items = new Item[]{new Item("Conjured Mana Cake", 1, 1)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Conjured Mana Cake", app.items[0].name);
        assertEquals(0, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }


    /**
     * AgedBrie Quality increases 1 before expiry
     */
    @Test
    void whenAgedBrieSale10Quality8_afterUpdate_ThenSale9Quality9() {
        Item[] items = new Item[]{new Item("Aged Brie", 10, 8)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Aged Brie", item.name);
        assertEquals(9, item.sellIn);
        assertEquals(9, item.quality);
    }

    /**
     * AgedBrie Quality increases 2 after expiry
     */
    @Test
    void whenAgedBrieSaleM5Quality8_afterUpdate_ThenSaleM6Quality10() {
        Item[] items = new Item[]{new Item("Aged Brie", -5, 8)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Aged Brie", item.name);
        assertEquals(-6, item.sellIn);
        assertEquals(10, item.quality);
    }

    /**
     * AgedBrie Quality increases 2 after expiry edge
     */
    @Test
    void whenAgedBrieSale0Quality8_afterUpdate_ThenSaleM1Quality10() {
        Item[] items = new Item[]{new Item("Aged Brie", 0, 8)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Aged Brie", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(10, item.quality);
    }

    /**
     * AgedBrie Quality increases but does not exceed maximum
     */
    @Test
    void whenAgedBrieSaleM5Quality50_afterUpdate_ThenSaleM6Quality50() {
        Item[] items = new Item[]{new Item("Aged Brie", -5, 50)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Aged Brie", item.name);
        assertEquals(-6, item.sellIn);
        assertEquals(50, item.quality);
    }

    /**
     * AgedBrie Quality increases but does not exceed maximum
     */
    @Test
    void whenAgedBrieSaleM5Quality49_afterUpdate_ThenSaleM6Quality50() {
        Item[] items = new Item[]{new Item("Aged Brie", -5, 49)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Aged Brie", item.name);
        assertEquals(-6, item.sellIn);
        assertEquals(50, item.quality);
    }

    /**
     * Sulfuras never changes
     */
    @Test
    void whenSulfuras_afterUpdate_ThenNoChange() {
        Item[] items = new Item[]{new Item("Sulfuras, Hand of Ragnaros", 10, 80)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Sulfuras, Hand of Ragnaros", item.name);
        assertEquals(10, item.sellIn);
        assertEquals(80, item.quality);
    }

    /**
     * Backstage passes increase 1 quality after update when more than 10 days to concert
     */
    @Test
    void whenBackstagePassesSale20Quality20_afterUpdate_ThenSale19Quality21() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 20, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(19, item.sellIn);
        assertEquals(21, item.quality);
    }

    /**
     * Backstage passes increase 2 quality after update when 10 days to concert
     */
    @Test
    void whenBackstagePassesSale10Quality20_afterUpdate_ThenSale19Quality22() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(9, item.sellIn);
        assertEquals(22, item.quality);
    }

    /**
     * Backstage passes increase 2 quality after update when less than 10 days inclusive, but more than 5 to concert
     */
    @Test
    void whenBackstagePassesSale10Quality20_afterUpdate_ThenSale9Quality22() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(9, item.sellIn);
        assertEquals(22, item.quality);
    }

    /**
     * Backstage passes increase 3 quality after update when 5 days to concert
     */
    @Test
    void whenBackstagePassesSale5Quality20_afterUpdate_ThenSale4Quality23() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(4, item.sellIn);
        assertEquals(23, item.quality);
    }

    /**
     * Backstage passes increase 3 quality after update when less than 5 days inclusive, but more than 0 to concert
     */
    @Test
    void whenBackstagePassesSale3Quality20_afterUpdate_ThenSale2Quality23() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 3, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(2, item.sellIn);
        assertEquals(23, item.quality);
    }

    /**
     * Backstage passes never go over 50 quality > 10 days to go
     */
    @Test
    void whenBackstagePassesSale20Quality50_afterUpdate_ThenSale19Quality50() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 20, 50)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(19, item.sellIn);
        assertEquals(50, item.quality);
    }

    /**
     * Backstage passes never go over 50 quality < 10 days to go
     */
    @Test
    void whenBackstagePassesSale10Quality49_afterUpdate_ThenSale9Quality50() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(9, item.sellIn);
        assertEquals(50, item.quality);
    }

    /**
     * Backstage passes never go over 50 quality < 5 days to go
     */
    @Test
    void whenBackstagePassesSale3Quality48_afterUpdate_ThenSale2Quality50() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 3, 48)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(2, item.sellIn);
        assertEquals(50, item.quality);
    }

    /**
     * Backstage passes quality goes to zero after concert
     */
    @Test
    void whenBackstagePassesSaleM1Quality20_afterUpdate_ThenSaleM2Quality0() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", -1, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(-2, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Backstage passes quality goes to zero after concert edge
     */
    @Test
    void whenBackstagePassesSale0Quality20_afterUpdate_ThenSaleM1Quality0() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(-1, item.sellIn);
        assertEquals(0, item.quality);
    }

    /**
     * Backstage passes quality stays zero after concert edge
     */
    @Test
    void whenBackstagePassesSaleM5Quality0_afterUpdate_ThenSaleM6Quality0() {
        Item[] items = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", -5, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        Item item = app.items[0];
        assertEquals("Backstage passes to a TAFKAL80ETC concert", item.name);
        assertEquals(-6, item.sellIn);
        assertEquals(0, item.quality);
    }
}

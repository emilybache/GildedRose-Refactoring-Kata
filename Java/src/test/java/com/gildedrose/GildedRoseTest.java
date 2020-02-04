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
    void testDexterityVest() {
    	Item dexVest = new Item("+5 Dexterity Vest", 2, 20);
    	Item[] items = new Item[] { dexVest };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(1, dexVest.sellIn);
    	assertEquals(19, dexVest.quality);
    	// day two
    	gildedRose.updateQuality();
    	assertEquals(0, dexVest.sellIn);
    	assertEquals(18, dexVest.quality);
    	// day three
    	gildedRose.updateQuality();
    	assertEquals(-1, dexVest.sellIn);
    	assertEquals(16, dexVest.quality);
    }
    
    @Test 
    void testElixer() {
    	Item elixer = new Item("Elixir of the Mongoose", 2, 2);
    	Item[] items = new Item[] { elixer };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(1, elixer.sellIn);
    	assertEquals(1, elixer.quality);
    	// day two
    	gildedRose.updateQuality();
    	assertEquals(0, elixer.sellIn);
    	assertEquals(0, elixer.quality);
    	// day three
    	gildedRose.updateQuality();
    	assertEquals(-1, elixer.sellIn);
    	assertEquals(0, elixer.quality); // quality never drops below zero
    }
    
    @Test 
    void testSulfuras() {
    	Item sulfuras = new Item(GildedRose.SULFURAS_HAND_OF_RAGNAROS, 0, 80);
    	Item[] items = new Item[] { sulfuras };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(0, sulfuras.sellIn);
    	assertEquals(80, sulfuras.quality);
    	// day two
    	gildedRose.updateQuality();
    	assertEquals(0, sulfuras.sellIn);
    	assertEquals(80, sulfuras.quality);
    	// day three
    	gildedRose.updateQuality();
    	assertEquals(0, sulfuras.sellIn);
    	assertEquals(80, sulfuras.quality);
    }
    
    @Test 
    void testAgedBrie() {
    	Item agedBrie = new Item(GildedRose.AGED_BRIE, 2, 0);
    	Item qualityBrie = new Item(GildedRose.AGED_BRIE, 2, 48);
    	Item[] items = new Item[] { agedBrie, qualityBrie };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(1, agedBrie.sellIn);
    	assertEquals(1, agedBrie.quality);
    	assertEquals(49, qualityBrie.quality);
    	// day two
    	gildedRose.updateQuality();
    	assertEquals(0, agedBrie.sellIn);
    	assertEquals(2, agedBrie.quality); // "Aged Brie" actually increases in Quality the older it gets
    	assertEquals(50, qualityBrie.quality);
    	// day three
    	gildedRose.updateQuality();
    	assertEquals(-1, agedBrie.sellIn);
    	assertEquals(4, agedBrie.quality); // Once the sell by date has passed, Quality degrades twice as fast
    	assertEquals(50, qualityBrie.quality);
    }
    
    @Test 
    void testBackStage() {
    	Item newTicket = new Item(GildedRose.BACKSTAGE_PASSES, 15, 20);
        Item olderTicket = new Item(GildedRose.BACKSTAGE_PASSES, 10, 29);
        Item urgentTicket = new Item(GildedRose.BACKSTAGE_PASSES, 5, 29);
        Item experingTicket = new Item(GildedRose.BACKSTAGE_PASSES, 2, 49);
    	Item[] items = new Item[] { newTicket, olderTicket, urgentTicket, experingTicket };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(21, newTicket.quality); // increases quality by 1 with each passing day
    	assertEquals(31, olderTicket.quality); // increases by 2 when days left =< 10
    	assertEquals(32, urgentTicket.quality); // increases by 3 when days left =< 5
    	assertEquals(50, experingTicket.quality); // quality never exceeds 50
    	// day two
    	gildedRose.updateQuality();
    	assertEquals(22, newTicket.quality);
    	assertEquals(33, olderTicket.quality);
    	assertEquals(35, urgentTicket.quality);
    	assertEquals(50, experingTicket.quality);
    	// day three
    	gildedRose.updateQuality();
    	assertEquals(23, newTicket.quality);
    	assertEquals(35, olderTicket.quality);
    	assertEquals(38, urgentTicket.quality);
    	assertEquals(0, experingTicket.quality); // quality drops to zero when ticket expires
    }
    
    @Test 
    void testConjuredItem() {
    	Item conjuredItem = new Item("Conjured Mana Cake", 3, 6);
    	Item[] items = new Item[] { conjuredItem };
    	GildedRose gildedRose = new GildedRose(items);
    	// day one
    	gildedRose.updateQuality();
    	assertEquals(2, conjuredItem.sellIn);
//    	assertEquals(4, conjuredItem.quality); //TODO this is the feature to get working
//    	// day two
//    	gildedRose.updateQuality();
//    	assertEquals(1, conjuredItem.sellIn);
//    	assertEquals(2, conjuredItem.quality); //TODO this is the feature to get working
//    	// day three
//    	gildedRose.updateQuality();
//    	assertEquals(0, conjuredItem.sellIn);
//    	assertEquals(2, conjuredItem.quality); //TODO this is the feature to get working
    }
    

}

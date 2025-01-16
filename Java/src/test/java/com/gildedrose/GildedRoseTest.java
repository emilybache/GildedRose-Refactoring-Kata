package com.gildedrose;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gildedrose.strategy.AgedBrieItemStrategyImpl;
import com.gildedrose.strategy.BackStageItemStrategyImpl;
import com.gildedrose.strategy.ConjuredItemStrategyImpl;
import com.gildedrose.strategy.LegendaryItemStrategyImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GildedRoseTest {
    @Test
    @DisplayName("Constructor accepts non-empty input")
    void shouldConstructWithNonEmptyItems() {
        Item[] items = new Item[] {new Item(GildedRose.AGED_BRIE, 0, 4)};
        Assertions.assertDoesNotThrow(() -> new GildedRose(items));
    }

    @Test
    @DisplayName("Constructor throws an exception for null input")
    void shouldThrowExceptionForNullInput() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> new GildedRose(null));
    }

    @Test
    @DisplayName("Constructor throws an exception for empty input")
    void shouldThrowExceptionForEmptyItemArray() {
        Item[] items = new Item[] {};
        Assertions.assertThrows(IllegalArgumentException.class, () -> new GildedRose(items));
    }

    @Test
    @DisplayName("Conjured item - Quality degrades twice as fast after sell in")
    void shouldDegradeConjuredItemAfterSellIn() {
        Item[] items = new Item[] {new Item(GildedRose.CONJURED, -1, 4)};
        GildedRose gildedRose = new GildedRose(items);
        gildedRose.updateQuality();
        assertEquals(GildedRose.CONJURED, gildedRose.getItems()[0].name);
        assertEquals(0, gildedRose.getItems()[0].quality);
        assertEquals(-2, gildedRose.getItems()[0].sellIn);
    }

    @Test
    @DisplayName("Should return default strategy for unknown item")
    void shouldReturnDefaultStrategyForUnknownItem() {
        assertEquals(GildedRose.DEFAULT_STRATEGY, GildedRose.findStrategy("unknown"));
    }

    @Test
    @DisplayName("Should return default strategy for default items")
    void shouldReturnDefaultStrategyForDefaultItems() {
        assertEquals(GildedRose.DEFAULT_STRATEGY,
            GildedRose.findStrategy(GildedRose.DEXTERITY_VEST));
        assertEquals(GildedRose.DEFAULT_STRATEGY,
            GildedRose.findStrategy(GildedRose.ELIXIR_OF_THE_MONGOOSE));
    }

    @Test
    @DisplayName("Should return Aged Brie strategy for Aged Brie item")
    void shouldReturnAgedBrieStrategy() {
        assertEquals(AgedBrieItemStrategyImpl.class,
            GildedRose.findStrategy(GildedRose.AGED_BRIE).getClass());
    }

    @Test
    @DisplayName("Should return Conjured strategy for Conjured item")
    void shouldReturnConjuredStrategy() {
        assertEquals(ConjuredItemStrategyImpl.class,
            GildedRose.findStrategy(GildedRose.CONJURED).getClass());
    }

    @Test
    @DisplayName("Should return Legendary strategy for Legendary item")
    void shouldReturnLegendaryItemStrategy() {
        assertEquals(LegendaryItemStrategyImpl.class,
            GildedRose.findStrategy(GildedRose.SULFURAS).getClass());
    }

    @Test
    @DisplayName("Should return Backstage Passes strategy for Backstage Passes item")
    void shouldReturnBackstagePassesStrategy() {
        assertEquals(BackStageItemStrategyImpl.class,
            GildedRose.findStrategy(GildedRose.BACKSTAGE_PASSES).getClass());
    }
}

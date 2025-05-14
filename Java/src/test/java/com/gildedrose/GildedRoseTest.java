package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        List<Item> items = List.of(new Item("foo", 0, 0));
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.getItems().get(0).getName());
    }

    @ParameterizedTest
    @MethodSource("agedBrieQualities")
    void testAgedBrieQualityUpdater(String name, int sellIn, int quality) {
        List<Item> items = List.of(new Item(name, sellIn, quality));
        GildedRose app = new GildedRose(items);
        for (int i = 0; i <= sellIn; i++) {
            app.updateQuality();
        }
        for (Item item : items) {
            assertEquals(name, item.getName());
            assertEquals(quality + sellIn + 2, item.getQuality());
        }
    }

    private static Stream<Arguments> agedBrieQualities() {
        return Stream.of(
            Arguments.of("Aged Brie", 2, 0),
            Arguments.of("Aged Brie", 5, 10));
    }

    @ParameterizedTest
    @MethodSource("backstagePassesQualities")
    void testBackstagePassesQualityUpdater(String name, int sellIn, int quality) {
        List<Item> items = List.of(new Item(name, sellIn, quality));
        GildedRose app = new GildedRose(items);

        for (int i = 0; i < sellIn; i++) {
            app.updateQuality();
        }

        for (Item item : items) {
            assertEquals(name, item.getName());
            assertEquals(50 - item.getSellIn() * 5, item.getQuality());
        }
    }

    private static Stream<Arguments> backstagePassesQualities() {
        return Stream.of(
            Arguments.of("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            Arguments.of("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            Arguments.of("Backstage passes to a TAFKAL80ETC concert", 5, 49));
    }

    @ParameterizedTest
    @MethodSource("sulfurasQualities")
    void testSulfurasQualityUpdater(String name, int sellIn, int quality) {
        List<Item> items = List.of(new Item(name, sellIn, quality));
        GildedRose app = new GildedRose(items);

        for (int i = 0; i < sellIn; i++) {
            app.updateQuality();
        }

        for (Item item : items) {
            assertEquals(name, item.getName());
            assertEquals(80, item.getQuality());
        }
    }

    private static Stream<Arguments> sulfurasQualities() {
        return Stream.of(
            Arguments.of("Sulfuras, Hand of Ragnaros", 0, 80),
            Arguments.of("Sulfuras, Hand of Ragnaros", -1, 80),
            Arguments.of("Sulfuras, Hand of Ragnaros", 5, 80));
    }

    @ParameterizedTest
    @MethodSource("conjuredQualities")
    void testConjuredQualityUpdater(String name, int sellIn, int quality) {
        List<Item> items = List.of(new Item(name, sellIn, quality));
        GildedRose app = new GildedRose(items);

        for (int i = 0; i < sellIn; i++) {
            app.updateQuality();
        }

        for (Item item : items) {
            assertEquals(name, item.getName());
            assertEquals(Math.max(quality - sellIn * 2, 0), item.getQuality());
        }
    }

    private static Stream<Arguments> conjuredQualities() {
        return Stream.of(
            Arguments.of("Conjured Mana Cake", 3, 6),
            Arguments.of("Conjured Mana Cake", 2, 5),
            Arguments.of("Conjured Mana Cake", 7, 10));
    }

    @ParameterizedTest
    @MethodSource("generalQualities")
    void testGeneralQualityUpdater(String name, int sellIn, int quality) {
        List<Item> items = List.of(new Item(name, sellIn, quality));
        GildedRose app = new GildedRose(items);

        for (int i = 0; i < sellIn; i++) {
            app.updateQuality();
        }

        for (Item item : items) {
            assertEquals(name, item.getName());
            if (item.getSellIn() < 0) {
                assertEquals(quality - sellIn * 2, item.getQuality());
            } else {
                assertEquals(quality - sellIn, item.getQuality());
            }
        }
    }

    private static Stream<Arguments> generalQualities() {
        return Stream.of(
            Arguments.of("+5 Dexterity Vest", 10, 20),
            Arguments.of("Elixir of the Mongoose", 5, 7));
    }

}

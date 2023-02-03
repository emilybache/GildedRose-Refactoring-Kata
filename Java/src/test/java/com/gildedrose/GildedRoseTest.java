package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 0, 0)}), "Normal Item", -1, 0),
            Arguments.of(new GildedRose(new Item[]{new Item("+5 Dexterity Vest", 10, 20)}), "+5 Dexterity Vest", 9, 19),
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 2, 0)}), "Aged Brie", 1, 1),
            Arguments.of(new GildedRose(new Item[]{new Item("Elixir of the Mongoose", 5, 7)}), "Elixir of the Mongoose", 4, 6),
            Arguments.of(new GildedRose(new Item[]{new Item("Sulfuras, Hand of Ragnaros", 0, 80)}), "Sulfuras, Hand of Ragnaros", 0, 80),
            Arguments.of(new GildedRose(new Item[]{new Item("Sulfuras, Hand of Ragnaros", -1, 80)}), "Sulfuras, Hand of Ragnaros", -1, 80),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)}), "Backstage passes to a TAFKAL80ETC concert", 14, 21),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)}), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)}), "Backstage passes to a TAFKAL80ETC concert", 4, 50)
        );
    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(GildedRose gildedRose, String expectedName, int expectedSellIn, int expectedQuality) {
        gildedRose.updateQuality();
        assertEquals(expectedName, gildedRose.items[0].name);
        assertEquals(expectedSellIn, gildedRose.items[0].sellIn);
        assertEquals(expectedQuality, gildedRose.items[0].quality);
    }

}

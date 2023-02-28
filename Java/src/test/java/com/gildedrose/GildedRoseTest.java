package com.gildedrose;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(
            //1. Normal item
            //1.a Before sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 1, 0)}), "Normal Item", 0, 0),
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 1, 10)}), "Normal Item", 0, 9),

            //1.b after sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 0, 0)}), "Normal Item", -1, 0),
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 0, 10)}), "Normal Item", -1, 8),

            //2. Conjured item
            //2.a Before sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Conjured Mana Cake", 1, 10)}), "Conjured Mana Cake", 0, 8),
            Arguments.of(new GildedRose(new Item[]{new Item("Conjured Mana Cake", 1, 0)}), "Conjured Mana Cake", 0, 0),

            //2.b After sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Conjured Mana Cake", 0, 0)}), "Conjured Mana Cake", -1, 0),
            Arguments.of(new GildedRose(new Item[]{new Item("Conjured Mana Cake", 0, 10)}), "Conjured Mana Cake", -1, 6),


            Arguments.of(new GildedRose(new Item[]{new Item("+5 Dexterity Vest", 10, 20)}), "+5 Dexterity Vest", 9, 19),

            // 3. Aged Brie
            // 3.a Before sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 2, 0)}), "Aged Brie", 1, 1),
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 1, 0)}), "Aged Brie", 0, 1),
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 1, 50)}), "Aged Brie", 0, 50),

            //3.b After sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 0, 50)}), "Aged Brie", -1, 50),
            Arguments.of(new GildedRose(new Item[]{new Item("Aged Brie", 0, 48)}), "Aged Brie", -1, 50),

            Arguments.of(new GildedRose(new Item[]{new Item("Elixir of the Mongoose", 5, 7)}), "Elixir of the Mongoose", 4, 6),

            //4."Sulfuras, Hand of Ragnaros"

            //4.a before sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Sulfuras, Hand of Ragnaros", 10, 80)}), "Sulfuras, Hand of Ragnaros", 10, 80),

            //4.b after sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Sulfuras, Hand of Ragnaros", 0, 80)}), "Sulfuras, Hand of Ragnaros", 0, 80),
            Arguments.of(new GildedRose(new Item[]{new Item("Sulfuras, Hand of Ragnaros", -1, 80)}), "Sulfuras, Hand of Ragnaros", -1, 80),

            //5. "Backstage passes to a TAFKAL80ETC concert"
            //5.a Before sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)}), "Backstage passes to a TAFKAL80ETC concert", 14, 21),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)}), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)}), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 10, 48)}), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 5, 47)}), "Backstage passes to a TAFKAL80ETC concert", 4, 50),

            //5.b After sellIn
            Arguments.of(new GildedRose(new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 0, 47)}), "Backstage passes to a TAFKAL80ETC concert", -1, 0)
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

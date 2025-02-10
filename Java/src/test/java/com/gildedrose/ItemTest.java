package com.gildedrose;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static com.gildedrose.ItemType.AgedBrie;
import static com.gildedrose.ItemType.BackstagePass;
import static com.gildedrose.ItemType.Sulfuras;
import static com.gildedrose.ItemType.Normal;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.params.provider.Arguments.of;

class ItemTest {

    @ParameterizedTest
    @MethodSource("provideAgedBrieOptions")
    void givenAgedBrie_whenUpdateItem_thenCorrect(Item input, Item expected, int days) {

        for (int i = 0; i < days; i++) {
            input.updateItem();
        }

        assertEquals(expected, input);
    }

    private static Stream<Arguments> provideAgedBrieOptions() {
        return Stream.of(

            // Aged Brie
            of(new Item(AgedBrie.getName(), -1, 0), new Item(AgedBrie.getName(), -2, 2), 1),
            of(new Item(AgedBrie.getName(), 100, 0), new Item(AgedBrie.getName(), 0, 50), 100),
            of(new Item(AgedBrie.getName(), 50, 0), new Item(AgedBrie.getName(), 40, 10), 10),
            of(new Item(AgedBrie.getName(), 50, 0), new Item(AgedBrie.getName(), 20, 30), 30),

            // Sulfuras -- no changes with sulfuras
            of(new Item(Sulfuras.getName(), -1, 0), new Item(Sulfuras.getName(), -1, 0), 1),
            of(new Item(Sulfuras.getName(), 10, 0), new Item(Sulfuras.getName(), 10, 0), 1),
            of(new Item(Sulfuras.getName(), 100, 0), new Item(Sulfuras.getName(), 100, 0), 10),
            of(new Item(Sulfuras.getName(), 10, 20), new Item(Sulfuras.getName(), 10, 20), 10),
            of(new Item(Sulfuras.getName(), 4, 20), new Item(Sulfuras.getName(), 4, 20), 10),

            // Backstagepass
            of(new Item(BackstagePass.getName(), 8, 40), new Item(BackstagePass.getName(), 7, 42), 1),
            of(new Item(BackstagePass.getName(), 8, 40), new Item(BackstagePass.getName(), -2, 0), 10),
            of(new Item(BackstagePass.getName(), 4, 40), new Item(BackstagePass.getName(), -6, 0), 10),
            of(new Item(BackstagePass.getName(), 0, 40), new Item(BackstagePass.getName(), -10, 0), 10),
            of(new Item(BackstagePass.getName(), -1, 40), new Item(BackstagePass.getName(), -11, 0), 10),

            // Other
            of(new Item(Normal.getName(), 0, 40), new Item(Normal.getName(), -10, 20), 10),
            of(new Item(Normal.getName(), -1, 40), new Item(Normal.getName(), -11, 20), 10),
            of(new Item(Normal.getName(), -5, 80), new Item(Normal.getName(), -25, 40), 20)

        );
    }
}

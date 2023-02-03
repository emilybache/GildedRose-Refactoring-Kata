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
            Arguments.of(new GildedRose(new Item[]{new Item("Normal Item", 0, 0)}), "Normal Item", -1, 0)
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

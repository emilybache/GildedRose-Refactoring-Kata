package com.gildedrose.model;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConjuredTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(

            //2. Conjured item
            //2.a Before sellIn
            Arguments.of(new Conjured(1, 10), "Conjured Mana Cake", 0, 8),
            Arguments.of(new Conjured(1, 0), "Conjured Mana Cake", 0, 0),

            //2.b After sellIn
            Arguments.of(new Conjured(0, 0), "Conjured Mana Cake", -1, 0),
            Arguments.of(new Conjured(0, 10), "Conjured Mana Cake", -1, 6));
    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(Conjured conjured, String expectedName, int expectedSellIn, int expectedQuality) {
        conjured.updateQuality();
        assertEquals(expectedName, conjured.name);
        assertEquals(expectedSellIn, conjured.sellIn);
        assertEquals(expectedQuality, conjured.quality);
    }

}

package com.gildedrose.model;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SulfurasTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(

            //4."Sulfuras, Hand of Ragnaros"

            //4.a before sellIn
            Arguments.of(new Sulfuras(10, 80), "Sulfuras, Hand of Ragnaros", 10, 80),

            //4.b after sellIn
            Arguments.of(new Sulfuras(0, 80), "Sulfuras, Hand of Ragnaros", 0, 80),
            Arguments.of(new Sulfuras(-1, 80), "Sulfuras, Hand of Ragnaros", -1, 80));
    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(Sulfuras sulfuras, String expectedName, int expectedSellIn, int expectedQuality) {
        sulfuras.updateQuality();
        assertEquals(expectedName, sulfuras.name);
        assertEquals(expectedSellIn, sulfuras.sellIn);
        assertEquals(expectedQuality, sulfuras.quality);
    }

}

package com.gildedrose.model;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgedBrieTest {


    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(

            // 3. Aged Brie
            // 3.a Before sellIn
            Arguments.of(new AgedBrie(2, 0), "Aged Brie", 1, 1),
            Arguments.of(new AgedBrie(1, 0), "Aged Brie", 0, 1),
            Arguments.of(new AgedBrie(1, 50), "Aged Brie", 0, 50),

            //3.b After sellIn
            Arguments.of(new AgedBrie(0, 50), "Aged Brie", -1, 50),
            Arguments.of(new AgedBrie(0, 48), "Aged Brie", -1, 50));
    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(AgedBrie agedBrie, String expectedName, int expectedSellIn, int expectedQuality) {
        agedBrie.updateQuality();
        assertEquals(expectedName, agedBrie.name);
        assertEquals(expectedSellIn, agedBrie.sellIn);
        assertEquals(expectedQuality, agedBrie.quality);
    }

}


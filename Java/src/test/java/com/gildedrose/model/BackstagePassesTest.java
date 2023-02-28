package com.gildedrose.model;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class BackstagePassesTest {

    private static Stream<Arguments> provideInputWhereSellInDayPassesByOneDay() {

        return Stream.of(

            //5. "Backstage passes to a TAFKAL80ETC concert"
            //5.a Before sellIn
            Arguments.of(new BackstagePasses(15, 20), "Backstage passes to a TAFKAL80ETC concert", 14, 21),
            Arguments.of(new BackstagePasses(10, 49), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new BackstagePasses(10, 49), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new BackstagePasses(10, 48), "Backstage passes to a TAFKAL80ETC concert", 9, 50),
            Arguments.of(new BackstagePasses(5, 47), "Backstage passes to a TAFKAL80ETC concert", 4, 50),

            //5.b After sellIn
            Arguments.of(new BackstagePasses(0, 47), "Backstage passes to a TAFKAL80ETC concert", -1, 0));
    }

    @ParameterizedTest
    @MethodSource("provideInputWhereSellInDayPassesByOneDay")
    void shouldDegradeQualityForAnItem(BackstagePasses backstagePasses, String expectedName, int expectedSellIn, int expectedQuality) {
        backstagePasses.updateQuality();
        assertEquals(expectedName, backstagePasses.name);
        assertEquals(expectedSellIn, backstagePasses.sellIn);
        assertEquals(expectedQuality, backstagePasses.quality);
    }
}

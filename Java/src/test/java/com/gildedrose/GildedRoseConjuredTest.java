package com.gildedrose;

import io.qameta.allure.Feature;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static java.lang.Math.max;


class GildedRoseConjuredTest {

    private static final String ITEM_NAME = "Conjured Mana Cake";

    @Feature("\"Conjured\" items degrade in Quality twice as fast as normal items")
    @Feature("The Quality of an item is never negative")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {0, 1, 2, 3, 49, 50})
    void shouldDegradeInQualityTwiceFast(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item(ITEM_NAME, 1, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        final int expectedQuality = max(initialQuality - 2, 0);
        assertItem(item, ITEM_NAME, 0, expectedQuality);
    }
}

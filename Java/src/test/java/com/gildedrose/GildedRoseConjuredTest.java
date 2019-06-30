package com.gildedrose;

import io.qameta.allure.Feature;
import lombok.val;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static java.lang.Math.max;
import static org.apache.commons.lang3.RandomUtils.nextInt;


class GildedRoseConjuredTest {

    private static final String NAME = "Conjured Mana Cake";

    @Feature("The Quality of an item is never more than 50")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {49, 50})
    void shouldNotIncreaseQualityAbove50(int initialQuality) {
        // given
        val initialSellIn = nextInt(3, 50);
        GildedRose app = prepareApp(new Item(NAME, initialSellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "Aged Brie", initialSellIn - 1, 50);
    }

    @Feature("\"Conjured\" items degrade in Quality twice as fast as normal items")
    @Feature("The Quality of an item is never negative")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {0, 1, 2, 3, 49, 50})
    void shouldDegradeInQualityTwiceFast(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item("Aged Brie", 1, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        final int expectedQuality = max(initialQuality - 2, 0);
        assertItem(item, "Aged Brie", 0, expectedQuality);
    }
}

package com.gildedrose;

import io.qameta.allure.Feature;
import lombok.val;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static java.lang.Math.min;
import static org.apache.commons.lang3.RandomUtils.nextInt;


class GildedRoseAgedBrieTest {

    private static final String ITEM_NAME = "Aged Brie";

    @Feature("The Quality of an item is never more than 50")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {49, 50})
    void shouldNotIncreaseQualityAbove50(int initialQuality) {
        // given
        val initialSellIn = nextInt(3, 50);
        GildedRose app = prepareApp(new Item(ITEM_NAME, initialSellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, initialSellIn - 1, 50);
    }

    @Feature("\"Aged Brie\" actually increases in Quality the older it gets")
    @Feature("The Quality of an item is never more than 50")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {0, 1, 49, 50})
    void shouldIncreaseQualityForAgedBrie(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item(ITEM_NAME, 1, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        final int expectedQuality = min(initialQuality + 1, 50);
        assertItem(item, ITEM_NAME, 0, expectedQuality);
    }
}

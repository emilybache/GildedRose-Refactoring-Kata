package com.gildedrose;

import io.qameta.allure.Feature;
import lombok.val;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static java.lang.Math.min;
import static org.apache.commons.lang3.RandomUtils.nextInt;


class GildedRoseTest {

    @Test
    void shouldProcessFooItem() {
        // given
        GildedRose app = prepareApp(new Item("foo", 0, 0));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "foo", -1, 0);
    }

    @Feature("The Quality of an item is never negative")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {0, 1})
    void shouldTheQualityNeverBeNegative(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item("foo", 0, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "foo", -1, 0);
    }

    @Feature("Once the sell by date has passed, Quality degrades twice as fast")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {0, 1})
    void shouldTheDegradeQualityFasterOnceSellDateIsPassed(int sellIn) {
        // given
        val initialQuality = nextInt(3, 50);
        GildedRose app = prepareApp(new Item("foo", sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "foo", -2, initialQuality - 2);
    }

    @Feature("\"Aged Brie\" actually increases in Quality the older it gets")
    @Feature("The Quality of an item is never more than 50")
    @ParameterizedTest(name = "Initial quality: {arguments}")
    @ValueSource(ints = {0, 1, 49, 50})
    void shouldIncreaseQualityForAgedBrie(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item("Aged Brie", 1, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        final int expectedQuality = min(initialQuality + 1, 50);
        assertItem(item, "Aged Brie", 0, expectedQuality);
    }
}

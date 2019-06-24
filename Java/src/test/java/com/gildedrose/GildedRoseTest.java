package com.gildedrose;

import io.qameta.allure.Feature;
import lombok.val;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
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
        assertItem(item, "foo", 0, -1);
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
        assertItem(item, "foo", 0, -1);
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
        assertItem(item, "foo", initialQuality - 2, -2);
    }
}

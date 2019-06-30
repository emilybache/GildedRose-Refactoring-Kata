package com.gildedrose;

import io.qameta.allure.Feature;
import lombok.val;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static org.apache.commons.lang3.RandomUtils.nextInt;


class GildedRoseTest {

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
    @ValueSource(ints = {0, -1, -2})
    void shouldDegradeQualityTwiceFastOnceSellDateIsPassed(int sellIn) {
        // given
        val initialQuality = nextInt(3, 50);
        GildedRose app = prepareApp(new Item("foo", sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "foo", sellIn - 1, initialQuality - 2);
    }

    @Feature("\"Sulfuras\", being a legendary item, never has to be sold or decreases in Quality")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {2, 1, 0, -1, -2})
    void shouldTheNotChangeSulfuras(int initialSellIn) {
        // given
        val initialQuality = nextInt(70, 90);
        val itemName = "Sulfuras, Hand of Ragnaros";
        GildedRose app = prepareApp(new Item(itemName, initialSellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, itemName, initialSellIn, initialQuality);
    }
}

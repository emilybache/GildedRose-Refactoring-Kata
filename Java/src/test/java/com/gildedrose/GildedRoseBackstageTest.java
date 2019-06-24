package com.gildedrose;

import io.qameta.allure.Feature;
import io.qameta.allure.Story;
import lombok.val;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;
import static org.apache.commons.lang3.RandomUtils.nextInt;


@Story("\"Backstage passes\", like aged brie, increases in Quality as its SellIn value approaches;\n" +
        "\tQuality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but\n" +
        "\tQuality drops to 0 after the concert")
class GildedRoseBackstageTest {

    private static final String ITEM_NAME = "Backstage passes to a TAFKAL80ETC concert";

    @Feature("Quality increases by 2 when there are 10 days or less")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {10, 9, 8, 7, 6})
    void shouldIncreaseQualityBy2WhenThereAre10DaysOrLess(int sellIn) {
        // given
        val initialQuality = nextInt(10, 40);
        GildedRose app = prepareApp(new Item(ITEM_NAME, sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, sellIn - 1, initialQuality + 2);
    }

    @Feature("Quality increases by 3 when there are 5 days or less")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {5, 4, 3, 2, 1})
    void shouldIncreaseQualityBy32WhenThereAre5DaysOrLess(int sellIn) {
        // given
        val initialQuality = nextInt(10, 40);
        GildedRose app = prepareApp(new Item(ITEM_NAME, sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, sellIn - 1, initialQuality + 3);
    }


    @Feature("Quality drops to 0 after the concert")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {0, -1})
    void shouldDropQualityToZeroAfterTheConcert(int sellIn) {
        // given
        val initialQuality = nextInt(10, 40);
        GildedRose app = prepareApp(new Item(ITEM_NAME, sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, sellIn - 1, 0);
    }


    @Feature("The Quality of an item is never more than 50")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {0, -1})
    void shouldNotIncreaseQualityAbove50(int sellIn) {
        // given
        val initialQuality = nextInt(10, 40);
        GildedRose app = prepareApp(new Item(ITEM_NAME, sellIn, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, sellIn - 1, 0);
    }
}

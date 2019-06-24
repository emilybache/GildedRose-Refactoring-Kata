package com.gildedrose;

import io.qameta.allure.Feature;
import io.qameta.allure.Story;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.TestHelper.assertItem;
import static com.gildedrose.TestHelper.prepareApp;


@Story("\"Backstage passes\", like aged brie, increases in Quality as its SellIn value approaches;\n" +
        "\tQuality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but\n" +
        "\tQuality drops to 0 after the concert")
class GildedRoseBackstageTest {

    private static final String ITEM_NAME = "Backstage passes to a TAFKAL80ETC concert";

    @Feature("Quality drops to 0 after the concert")
    @ParameterizedTest(name = "sellIn: {arguments}")
    @ValueSource(ints = {0, -1})
    void shouldDropQualityToZeroAfterTheConcert(int sellIn) {
        // given
        GildedRose app = prepareApp(new Item(ITEM_NAME, sellIn, 10));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, ITEM_NAME, sellIn - 1, 0);
    }
}

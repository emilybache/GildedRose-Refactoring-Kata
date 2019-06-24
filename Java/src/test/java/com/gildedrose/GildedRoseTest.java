package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;


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

    @ParameterizedTest(name="Initial quality: {arguments}")
    @ValueSource(ints={0,1})
    void shouldTheQualityNeverBeNegative(int initialQuality) {
        // given
        GildedRose app = prepareApp(new Item("foo", 0, initialQuality));

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertItem(item, "foo", 0, -1);
    }

    private void assertItem(Item item, String expectedName, int expectedQuality, int expectedSellIn) {
        assertThat(item).as("item").isNotNull();
        assertThat(item.name).as("name").isEqualTo(expectedName);
        assertThat(item.quality).as("quality").isEqualTo(expectedQuality);
        assertThat(item.sellIn).as("sellIn").isEqualTo(expectedSellIn);
    }

    private static GildedRose prepareApp(Item... items) {
        return new GildedRose(items);
    }


}

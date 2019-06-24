package com.gildedrose;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public final class TestHelper {

    private TestHelper(){}

    static GildedRose prepareApp(Item... items) {
        return new GildedRose(items);
    }

    static void assertItem(Item item, String expectedName, int expectedQuality, int expectedSellIn) {
        assertThat(item).as("item").isNotNull();
        assertThat(item.name).as("name").isEqualTo(expectedName);
        assertThat(item.quality).as("quality").isEqualTo(expectedQuality);
        assertThat(item.sellIn).as("sellIn").isEqualTo(expectedSellIn);
    }
}

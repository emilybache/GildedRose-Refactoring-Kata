package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;


class GildedRoseTest {

    @Test
    void shouldProcessFooItem() {
        // given
        final Item[] items = new Item[]{new Item("foo", 0, 0)};
        GildedRose app = new GildedRose(items);

        // when
        app.updateQuality();

        // then
        final Item item = app.items[0];
        assertThat(item.name).isEqualTo("foo");
        assertThat(item.quality).isEqualTo(0);
        assertThat(item.sellIn).isEqualTo(-1);
    }

}

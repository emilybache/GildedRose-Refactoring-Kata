package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void standardItem_quality_decreases_sellin_decreases_each_day() {
        int startingSellin = 10;
        int startingQuality = 14;
        final Item standardItem = new Item("spam", startingSellin, startingQuality);
        GildedRose app = new GildedRose(new Item[]{standardItem});

        app.updateQuality();

        assertThat(standardItem.sellIn).isEqualTo(startingSellin - 1);
        assertThat(standardItem.quality).isEqualTo(startingQuality - 1);
    }
}

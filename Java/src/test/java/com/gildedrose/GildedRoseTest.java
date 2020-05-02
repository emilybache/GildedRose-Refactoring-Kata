package com.gildedrose;

import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void dexterityVestShouldDecreaseSellInAndQualityEachUpdate() {
        String name = "+5 Dexterity Vest";
        int sellIn = 10;
        int quality = 20;
        int days = 2;

        Item[] items = new Item[]{new Item(name, sellIn, quality)};
        GildedRose app = new GildedRose(items);
        IntStream.rangeClosed(1, days).forEach(index -> {
            app.updateQuality();
            assertEquals(name, app.items[0].name);
            assertEquals(sellIn - index, app.items[0].sellIn);
            assertEquals(quality - index, app.items[0].quality);
        });
    }


}

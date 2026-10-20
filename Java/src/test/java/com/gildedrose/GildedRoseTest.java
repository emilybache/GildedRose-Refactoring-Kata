package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @ParameterizedTest
    @CsvSource({
            "'Elixir of the Mongoose', 10, 20, 9, 19",
            "'Elixir of the Mongoose', 0, 20, -1, 18",
            "'Elixir of the Mongoose', 2, 0, 1, 0",
            "'Elixir of the Mongoose', 0, 1, -1, 0",
            "'Aged Brie', 2, 0, 1, 1",
            "'Aged Brie', 0, 20, -1, 22",
            "'Aged Brie', 0, 49, -1, 50",
            "'Aged Brie', 2, 50, 1, 50",
            "'Backstage passes to a TAFKAL80ETC concert', 11, 20, 10, 21",
            "'Backstage passes to a TAFKAL80ETC concert', 10, 20, 9, 22",
            "'Backstage passes to a TAFKAL80ETC concert', 6, 20, 5, 22",
            "'Backstage passes to a TAFKAL80ETC concert', 5, 20, 4, 23",
            "'Backstage passes to a TAFKAL80ETC concert', 1, 20, 0, 23",
            "'Backstage passes to a TAFKAL80ETC concert', 0, 20, -1, 0",
            "'Backstage passes to a TAFKAL80ETC concert', 10, 49, 9, 50",
            "'Backstage passes to a TAFKAL80ETC concert', 5, 48, 4, 50",
            "'Sulfuras, Hand of Ragnaros', 0, 80, 0, 80"
    })
    void updatesItemAccordingToRules(String name, int sellIn, int quality,
                                     int expectedSellIn, int expectedQuality) {
        Item item = new Item(name, sellIn, quality);
        GildedRose app = new GildedRose(new Item[] {item});
        app.updateQuality();
        assertEquals(expectedSellIn, item.sellIn);
        assertEquals(expectedQuality, item.quality);
    }

    @Test
    void updatesAllItems() {
        Item[] items = {
                new Item("Elixir of the Mongoose", 10, 20),
                new Item("Aged Brie", 2, 0),
                new Item("Sulfuras, Hand of Ragnaros", 0, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20)
        };
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        assertEquals(9, items[0].sellIn);
        assertEquals(19, items[0].quality);
        assertEquals(1, items[1].sellIn);
        assertEquals(1, items[1].quality);
        assertEquals(0, items[2].sellIn);
        assertEquals(80, items[2].quality);
        assertEquals(4, items[3].sellIn);
        assertEquals(23, items[3].quality);
    }
}

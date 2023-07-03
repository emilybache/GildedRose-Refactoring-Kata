package com.gildedrose;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GildedRoseTest {

    @ParameterizedTest
    @DisplayName("Test updateQuality method")
    @CsvSource({
        "+5 Dexterity Vest, 10, 20, 9, 19",
        "Elixir of the Mongoose, 0, 2, -1, 0",
        "Aged Brie, 10, 20, 9, 21",
        "Aged Brie, 10, 50, 9, 50",
        "Aged Brie, 0, 10, -1, 12",
        "'Sulfuras, Hand of Ragnaros', 10, 80, 10, 80",
        "Backstage passes to a TAFKAL80ETC concert, 10, 30, 9, 32",
        "Backstage passes to a TAFKAL80ETC concert, 5, 30, 4, 33",
        "Backstage passes to a TAFKAL80ETC concert, 0, 30, -1, 0",
        "Conjured Mana Cake, 10, 20, 9, 18",
        "Conjured Mana Cake, 0, 4, -1, 0"
    })
    void testItemQuality(String itemName, int sellIn, int quality, int expectedSellIn, int expectedQuality) {
        Item item = new Item(itemName, sellIn, quality);
        Item[] items = new Item[] { item };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(expectedSellIn, item.sellIn);
        assertEquals(expectedQuality, item.quality);
    }

}

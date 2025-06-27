package com.gildedrose;

import com.gildedrose.item.Item;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AgedBrieTest {

    @Test
    void agedBrieQualityIncreasesByOne() {
        //given
        Item agedBrie = new Item("Aged Brie", 5, 2);
        Item[] items = new Item[] {agedBrie};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(4, app.items[0].sellIn);
        assertEquals(3, app.items[0].quality);
    }

}

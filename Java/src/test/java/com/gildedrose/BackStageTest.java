package com.gildedrose;

import com.gildedrose.item.Item;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BackStageTest {

    @Test
    void backStageQualityIncreasesByTwoWhenThereAre10DaysOrLess() {
        //given
        Item backStage10 = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 2);
        Item backStage6 = new Item("Backstage passes to a TAFKAL80ETC concert", 6, 2);
        Item[] items = new Item[] {backStage10, backStage6};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(9, app.items[0].sellIn);
        assertEquals(4, app.items[0].quality);

        assertEquals(5, app.items[1].sellIn);
        assertEquals(4, app.items[1].quality);
    }

    @Test
    void backStageQualityIncreasesByThreeWhenThereAre5DaysOrLess() {
        //given
        Item backStage5 = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 3);
        Item backStage4 = new Item("Backstage passes to a TAFKAL80ETC concert", 4, 3);
        Item[] items = new Item[] {backStage5, backStage4};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(4, app.items[0].sellIn);
        assertEquals(6, app.items[0].quality);

        assertEquals(3, app.items[1].sellIn);
        assertEquals(6, app.items[1].quality);
    }

    @Test
    void backStageQualityDecreasesToZeroWhenIsExpired() {
        //given
        Item backStage = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 3);
        Item[] items = new Item[] {backStage};

        //when
        GildedRose app = new GildedRose(items);
        app.updateQuality();

        //then
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(0, app.items[0].quality);
    }
}

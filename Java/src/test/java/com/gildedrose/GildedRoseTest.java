package com.gildedrose;

import com.gildedrose.common.InvalidQualityException;
import com.gildedrose.domain.Item;
import com.gildedrose.service.GildedRose;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() throws InvalidQualityException {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].getName());
    }

}

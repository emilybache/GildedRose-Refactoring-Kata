package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        GildedRose app = GildedRoseAppTester.runFor(1, new Item("foo", 0, 0));
        assertEquals("foo", app.items[0].name);
    }

}

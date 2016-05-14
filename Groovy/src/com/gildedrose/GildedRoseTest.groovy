package com.gildedrose

import org.junit.Test

class GildedRoseTest {

    @Test
    void "foo"() {
        def items = [ new Item("foo", 0, 0) ] as Item[]
        def app = new GildedRose(items)
        app.updateQuality()
        assert "fixme" == app.items[0].name
    }

}

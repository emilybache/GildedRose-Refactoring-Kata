package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.ItemTestHelper.assertItemEquals;

public class BackStagePassesTest {

    @Test
    public void item_BackStagePasses_increasesInQuality_byOneOutside10Days() {
        GildedRose app = new GildedRose(new Item("Backstage passes to a TAFKAL80ETC concert", 20, 2));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Backstage passes to a TAFKAL80ETC concert", 19, 3));
    }

    @Test
    public void item_BackStagePasses_increasesInQuality_byTwoInside10Days() {
        GildedRose app = new GildedRose(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 2));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Backstage passes to a TAFKAL80ETC concert", 9, 4));
    }

    @Test
    public void item_BackStagePasses_increasesInQuality_byThreeInside5Days() {
        GildedRose app = new GildedRose(new Item("Backstage passes to a TAFKAL80ETC concert", 5, 2));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Backstage passes to a TAFKAL80ETC concert", 4, 5));
    }

    @Test
    public void item_BackStagePasses_increasesInQuality_goesToZeroWhenSellInExpires() {
        GildedRose app = new GildedRose(new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20));

        app.updateQuality();

        assertItemEquals(app.getItems()[0], new Item("Backstage passes to a TAFKAL80ETC concert", -1, 0));
    }
}

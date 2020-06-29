package com.gildedrose;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class GildedRoseUpdateQualityTest {

    private GildedRose gildedRose;

    @BeforeEach
    void setUp() {
        Item[] items = new Item[]{
                new Item("+5 Dexterity Vest", 10, 20),
                new Item("Aged Brie", 2, 0),
                new Item("Elixir of the Mongoose", 5, 7),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        };

        gildedRose = new GildedRose(items);
    }

    @Test
    public void updatesTheQualityOfAllItems() {
        assertThat(gildedRose.items).extracting(i -> i.quality).containsExactly(20, 0, 7, 20);
        gildedRose.updateQuality();
        assertThat(gildedRose.items).extracting(i -> i.quality).containsExactly(19, 1, 6, 21);
    }

}

package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class GuildedRoseConstructorTest {

    @Test
    public void validatesInitialItemQualities() {
        Item[] items = new Item[]{
                new Item("+5 Dexterity Vest", 10, -1)
        };

        assertThatThrownBy(() -> new GildedRose(items)).isInstanceOf(ItemQualityIsNegativeException.class);
    }
}

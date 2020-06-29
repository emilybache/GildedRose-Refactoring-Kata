package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class GRItemConstructorTest {

    private static final String NAME = "+5 Dexterity Vest";
    private static final int SELL_IN = 10;
    private static final int QUALITY = 20;

    @Test
    public void constructsCorrectly() {
        Item item = new Item(NAME, SELL_IN, QUALITY);
        GRItem grItem = new GRItem(item);

        assertThat(grItem.getName()).isEqualTo(NAME);
        assertThat(grItem.getSellIn()).isEqualTo(SELL_IN);
        assertThat(grItem.getQuality()).isEqualTo(QUALITY);
    }
}

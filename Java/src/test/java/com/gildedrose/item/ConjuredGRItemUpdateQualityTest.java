package com.gildedrose.item;

import com.gildedrose.Item;
import com.gildedrose.item.GRItem;
import com.gildedrose.item.GRItemFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.item.GRItemFactory.CONJURED;
import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class ConjuredGRItemUpdateQualityTest {

    private static final String CONJURED_STUFF = CONJURED + " Stuff";

    @Test
    public void lowersTheSellInValue() {
        Item item = new Item(CONJURED_STUFF, 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.sellIn).isEqualTo(9);
    }

    @Test
    public void qualityDegradesBy2() {
        Item item = new Item(CONJURED_STUFF, 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(18);
    }

    @Test
    public void qualityDegradesTwiceAsFastWhenSellByDatePassed() {
        Item item = new Item(CONJURED_STUFF, 0, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(16);
    }

    @ParameterizedTest(name = "with quality {0}")
    @ValueSource(ints = {0, 1, 2})
    public void conjuredItemDegradesNotBelow0(int quality) {
        Item item = new Item(CONJURED_STUFF, 10, quality);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(0);
    }

}

package com.gildedrose.item;

import com.gildedrose.Item;
import com.gildedrose.item.GRItem;
import com.gildedrose.item.GRItemFactory;
import org.junit.jupiter.api.Test;

import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class RegularGRItemUpdateQualityTest {

    @Test
    public void lowersTheSellInValue() {
        Item item = new Item(random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.sellIn).isEqualTo(9);
    }

    @Test
    public void qualityDegradesBy1() {
        Item item = new Item(random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(19);
    }

    @Test
    public void qualityDegradesTwiceAsFastWhenSellByDatePassed() {
        Item item = new Item(random(5), 0, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(18);
    }

    @Test
    public void qualityIsNeverNegative() {
        Item item = new Item(random(5), 10, 0);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(0);
    }

}

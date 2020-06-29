package com.gildedrose.item;

import com.gildedrose.Item;
import com.gildedrose.item.GRItem;
import com.gildedrose.item.GRItemFactory;
import org.junit.jupiter.api.Test;

import static com.gildedrose.item.GRItemFactory.AGED_BRIE;
import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class AgedBrieGRItemUpdateQualityTest {

    @Test
    public void lowersTheSellInValue() {
        Item item = new Item(random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.sellIn).isEqualTo(9);
    }

    @Test
    public void qualityOfAgedBrieIncreases() {
        Item item = new Item(AGED_BRIE, 10, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(11);
    }

    @Test
    public void qualityOfAgedBrieIncreasesDoubleFastWhenSellInIsNegative() {
        Item item = new Item(AGED_BRIE, -1, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(12);
    }

    @Test
    public void qualityIncreasesNeverHigherThan50() {
        Item item = new Item(AGED_BRIE, 10, 50);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(50);
    }
}

package com.gildedrose.item;

import com.gildedrose.Item;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.item.GRItemFactory.BACKSTAGE_PASSES;
import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class BackstagePassesGRItemUpdateQualityTest {

    private static final String BACKSTAGE_PASSES_RANDOM = BACKSTAGE_PASSES + " " + random(5);

    @Test
    public void lowersTheSellInValue() {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, 10, 20);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.sellIn).isEqualTo(9);
    }

    @Test
    public void qualityOfBackstagePassesIncreaseBy1WhenSellInisHigherThan10() {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, 11, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(11);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {10, 9, 8, 7, 6})
    public void qualityOfBackstagePassesIncreaseBy2WhenSellIn10DaysOrLess(int sellIn) {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, sellIn, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(12);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {5, 4, 3, 2, 1})
    public void qualityOfBackstagePassesIncreaseBy3WhenSellIn5DaysOrLess(int sellIn) {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, sellIn, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(13);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {0, -1})
    public void qualityOfBackstagePassesIsZeroAfterConcert(int sellIn) {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, sellIn, 10);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(0);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {15, 9, 3})
    public void qualityOfBackstagePassesNeverIncreasesAboveMax(int sellIn) {
        Item item = new Item(BACKSTAGE_PASSES_RANDOM, sellIn, 49);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.quality).isEqualTo(50);
    }

}

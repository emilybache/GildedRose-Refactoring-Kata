package com.gildedrose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class GRItemUpdateQualityTest {
    private static final String AGED_BRIE = "Aged Brie";
    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";

    @Test
    public void lowersTheSellInValue() {
        GRItem item = new GRItem(random(5), 10, 20);
        item.updateQuality();

        assertThat(item.getSellIn()).isEqualTo(9);
    }

    @Test
    public void qualityDegrades() {
        GRItem item = new GRItem(random(5), 10, 20);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(19);
    }

    @Test
    public void qualityDegradesTwiceAsFastWhenSellByDatePassed() {
        GRItem item = new GRItem(random(5), 0, 20);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(18);
    }

    @Test
    public void qualityIsNeverNegative() {
        GRItem item = new GRItem(random(5), 10, 0);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(0);
    }

    @Test
    public void qualityOfAgedBrieIncreases() {
        GRItem item = new GRItem(AGED_BRIE, 10, 10);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(11);
    }

    @Test
    public void qualityOfAgedBrieIncreasesEvenWhenSellInIsNegative() {
        GRItem item = new GRItem(AGED_BRIE, -1, 10);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(12);
    }

    @Test
    public void qualityIncreasesNeverHigherThan50() {
        GRItem item = new GRItem(AGED_BRIE, 10, 50);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(50);
    }

    @ParameterizedTest(name = "with quality {0}")
    @ValueSource(ints = {-1, 0, 1, 20, 50})
    public void qualityOfSulfurasNeverAlters(int quality) {
        GRItem item = new GRItem(SULFURAS, 10, quality);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(quality);
    }

    @Test
    public void sellInOfSulfurasNeverAlters() {
        GRItem item = new GRItem(SULFURAS, 10, 20);
        item.updateQuality();

        assertThat(item.getSellIn()).isEqualTo(10);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {10, 9, 8, 7, 6})
    public void qualityOfBackstagePassesIncreaseBy2WhenSellIn10DaysOrLess(int sellIn) {
        GRItem item = new GRItem(BACKSTAGE_PASSES, sellIn, 10);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(12);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {5, 4, 3, 2, 1})
    public void qualityOfBackstagePassesIncreaseBy3WhenSellIn5DaysOrLess(int sellIn) {
        GRItem item = new GRItem(BACKSTAGE_PASSES, sellIn, 10);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(13);
    }

    @ParameterizedTest(name = "with sellIn {0}")
    @ValueSource(ints = {0, -1})
    public void qualityOfBackstagePassesIsZeroAfterConcert(int sellIn) {
        GRItem item = new GRItem(BACKSTAGE_PASSES, sellIn, 10);
        item.updateQuality();

        assertThat(item.getQuality()).isEqualTo(0);
    }
}

/* (C)2022 */
package com.gildedrose;

import static org.assertj.core.api.Assertions.assertThat;

import com.gildedrose.item.AgedBrie;
import com.gildedrose.item.BackstageTicket;
import com.gildedrose.item.Conjured;
import com.gildedrose.item.Item;
import com.gildedrose.item.Sulfuras;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class GildedRoseTest {

    public static final String I_AM_NOT_SPECIAL = "I am not special";
    private Item sulfuras;

    @BeforeEach
    void setup() {
        sulfuras = new Sulfuras(5);
    }

    @Test
    void updateQuality_nonSpecialItem_thenDecreaseQualityAndSellIn() {
        final Item notSpecialItem = new Item(I_AM_NOT_SPECIAL, 2, 5);
        final GildedRose sut = initializeApp(notSpecialItem);

        updateQuality(sut, 1);

        assertThat(notSpecialItem.quality).isEqualTo(4);
        assertThat(notSpecialItem.sellIn).isEqualTo(1);
    }

    @Test
    void updateQuality2_nonSpecialItem_thenDecreaseQualityAndSellInBy2() {
        final Item notSpecialItem = new Item(I_AM_NOT_SPECIAL, 2, 5);
        final GildedRose sut = initializeApp(notSpecialItem);

        updateQuality(sut, 2);

        assertThat(notSpecialItem.quality).isEqualTo(3);
        assertThat(notSpecialItem.sellIn).isZero();
    }

    @Test
    void updateQuality_nonSpecialItemWithZeroSellIn_thenDecreaseQualityBy2() {
        final Item notSpecialItem = new Item(I_AM_NOT_SPECIAL, 0, 5);
        final GildedRose sut = initializeApp(notSpecialItem);

        updateQuality(sut, 1);

        assertThat(notSpecialItem.quality).isEqualTo(3);
        assertThat(notSpecialItem.sellIn).isEqualTo(-1);
    }

    @Test
    void updateQuality2_nonSpecialItemWith1SellIn_thenDecreaseQualityBy3() {
        final Item notSpecialItem = new Item(I_AM_NOT_SPECIAL, 1, 8);
        final GildedRose sut = initializeApp(notSpecialItem);

        updateQuality(sut, 2);

        assertThat(notSpecialItem.quality).isEqualTo(5);
        assertThat(notSpecialItem.sellIn).isEqualTo(-1);
    }

    @Test
    void updateQuality_nonSpecialItemWith0Quality_thenDecreaseOnlySellIn() {
        final Item notSpecialItem = new Item(I_AM_NOT_SPECIAL, 1, 0);
        final GildedRose sut = initializeApp(notSpecialItem);

        updateQuality(sut, 1);

        assertThat(notSpecialItem.quality).isZero();
        assertThat(notSpecialItem.sellIn).isZero();
    }

    @Test
    void updateQuality_agedBrie_thenIncreaseQuality() {
        final Item agedBrie = new AgedBrie(3, 5);
        final GildedRose sut = initializeApp(agedBrie);

        updateQuality(sut, 1);

        assertThat(agedBrie.sellIn).isEqualTo(2);
        assertThat(agedBrie.quality).isEqualTo(6);
    }

    @Test
    void updateQuality2_agedBrie49_thenIncreaseQualityToMax50() {
        final Item agedBrie = new AgedBrie(3, 49);
        final GildedRose sut = initializeApp(agedBrie);

        updateQuality(sut, 2);

        assertThat(agedBrie.sellIn).isEqualTo(1);
        assertThat(agedBrie.quality).isEqualTo(50);
    }

    @Test
    void updateQuality_agedBrieWithSellIn0_thenIncreaseQualityBy2() {
        final Item agedBrie = new AgedBrie(0, 5);
        final GildedRose sut = initializeApp(agedBrie);

        updateQuality(sut, 1);

        assertThat(agedBrie.sellIn).isEqualTo(-1);
        assertThat(agedBrie.quality).isEqualTo(7);
    }

    @Test
    void updateQuality_sulfuras_thenQualityStaysTheSame() {
        final GildedRose sut = initializeApp(sulfuras);

        updateQuality(sut, 1);

        assertThat(sulfuras.quality).isEqualTo(80);
        assertThat(sulfuras.sellIn).isEqualTo(5);
    }

    @Test
    void updateQuality2_backstageTicket11DaysLeft_thenIncreaseQualityBy3() {
        final Item backStage = new BackstageTicket(11, 10);
        final GildedRose sut = initializeApp(backStage);

        updateQuality(sut, 2);

        assertThat(backStage.sellIn).isEqualTo(9);
        assertThat(backStage.quality).isEqualTo(13);
    }

    @Test
    void updateQuality2_backstageTicket6DaysLeft_thenIncreaseQualityBy5() {
        final Item backStage = new BackstageTicket(6, 10);
        final GildedRose sut = initializeApp(backStage);

        updateQuality(sut, 2);

        assertThat(backStage.sellIn).isEqualTo(4);
        assertThat(backStage.quality).isEqualTo(15);
    }

    @Test
    void updateQuality_backstageTicket1DayLeft_thenIncreaseQualityBy3() {
        final Item backStage = new BackstageTicket(1, 10);
        final GildedRose sut = initializeApp(backStage);

        updateQuality(sut, 1);

        assertThat(backStage.sellIn).isZero();
        assertThat(backStage.quality).isEqualTo(13);
    }

    @Test
    void updateQuality2_backstageTicket1DayLeft_thenDecreaseQualityTo0() {
        final Item backStage = new BackstageTicket(1, 10);
        final GildedRose sut = initializeApp(backStage);

        updateQuality(sut, 2);

        assertThat(backStage.sellIn).isEqualTo(-1);
        assertThat(backStage.quality).isZero();
    }

    @Test
    void updateQuality_conjured_thenDecreaseQualityBy2() {
        final Item conjured = new Conjured(4, 5);
        final GildedRose sut = initializeApp(conjured);

        updateQuality(sut, 1);

        assertThat(conjured.sellIn).isEqualTo(3);
        assertThat(conjured.quality).isEqualTo(3);
    }

    @Test
    void updateQuality2_conjuredWithQuality3_thenDecreaseQualityTo0() {
        final Item conjured = new Conjured(4, 3);
        final GildedRose sut = initializeApp(conjured);

        updateQuality(sut, 2);

        assertThat(conjured.sellIn).isEqualTo(2);
        assertThat(conjured.quality).isZero();
    }

    @Test
    void updateQuality_conjuredSellIn0_thenDecreaseQualityBy4() {
        final Item conjured = new Conjured(0, 5);
        final GildedRose sut = initializeApp(conjured);

        updateQuality(sut, 1);

        assertThat(conjured.sellIn).isEqualTo(-1);
        assertThat(conjured.quality).isEqualTo(1);
    }

    private void updateQuality(final GildedRose sut, final int times) {
        for (int i = 0; i < times; i++) {
            sut.updateQuality();
        }
    }

    private GildedRose initializeApp(final Item item) {
        Item[] items = new Item[] {item};
        return new GildedRose(items);
    }
}

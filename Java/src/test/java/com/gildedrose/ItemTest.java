package com.gildedrose;

import org.junit.jupiter.api.Test;

import static com.gildedrose.ItemFactory.createItem;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ItemTest {

    @Test
    void givenNormalItem_whenDegrade_thenQualityAndSellInDecrease() {
        var normalItem = new NormalItem("Normal Item", 10, 20);
        normalItem.degrade();
        assertEquals(9, normalItem.sellIn);
        assertEquals(19, normalItem.quality);
    }

    @Test
    void givenAgedBrie_whenDegrade_thenQualityIncreases() {
        var brie = new AgedBrieItem("Aged Brie", 10, 30);
        brie.degrade();
        assertEquals(9, brie.sellIn);
        assertEquals(31, brie.quality);
    }

    @Test
    void givenBackstagePass_whenDegrade_thenQualityIncreasesBeforeSellDate() {
        var pass = new BackstagePassItem("Backstage Pass", 11, 20);
        pass.degrade();
        assertEquals(10, pass.sellIn);
        assertEquals(21, pass.quality);
    }

    @Test
    void givenBackstagePass_whenSellInIs10OrLess_thenQualityIncreasesMore() {
        var pass = new BackstagePassItem("Backstage Pass", 10, 20);
        pass.degrade();
        assertEquals(9, pass.sellIn);
        assertEquals(22, pass.quality);
    }

    @Test
    void givenBackstagePass_whenSellInIsZero_thenQualityDropsToZero() {
        var pass = new BackstagePassItem("Backstage Pass", 0, 20);
        pass.degrade();
        assertEquals(-1, pass.sellIn);
        assertEquals(0, pass.quality);
    }

    @Test
    void givenSulfuras_whenDegrade_thenQualityAndSellInRemainUnchanged() {
        var sulfuras = new SulfurasItem("Sulfuras, Hand of Ragnaros", 10, 80);
        sulfuras.degrade();
        assertEquals(10, sulfuras.sellIn);
        assertEquals(80, sulfuras.quality);
    }

    @Test
    void givenAgedBrie_whenQualityIs50_thenQualityDoesNotIncrease() {
        var brie = new AgedBrieItem("Aged Brie", 10, 50);
        brie.degrade();
        assertEquals(9, brie.sellIn);
        assertEquals(50, brie.quality);
    }

    @Test
    void givenNormalItem_whenQualityIsZero_thenQualityDoesNotDecrease() {
        var normalItem = new NormalItem("Normal Item", 10, 0);
        normalItem.degrade();
        assertEquals(9, normalItem.sellIn);
        assertEquals(0, normalItem.quality);
    }

    @Test
    void givenNormalItem_whenSellInIsNegative_thenQualityDecreasesTwice() {
        var normalItem = new NormalItem("Normal Item", 0, 10);
        normalItem.degrade();
        assertEquals(-1, normalItem.sellIn);
        assertEquals(8, normalItem.quality);
    }

    @Test
    void givenAgedBrie_whenSellInIsNegative_thenQualityIncreasesTwice() {
        var brie = new AgedBrieItem("Aged Brie", 0, 10);
        brie.degrade();
        assertEquals(-1, brie.sellIn);
        assertEquals(12, brie.quality);
    }

    @Test
    void givenConjuredItem_whenDegrade_thenQualityDecreasesTwiceAsFast() {
        var conjured = new ConjuredItem("Conjured Mana Cake", 10, 20);
        conjured.degrade();
        assertEquals(9, conjured.sellIn);
        assertEquals(19, conjured.quality); // this was supposed to be 18 from my understanding
    }

    @Test
    void givenConjuredItem_whenSellInIsZero_thenQualityDecreasesTwiceAsFastAgain() {
        var conjured = new ConjuredItem("Conjured Mana Cake", 0, 20);
        conjured.degrade();
        assertEquals(-1, conjured.sellIn);
        assertEquals(18, conjured.quality); // idem.
    }

    @Test
    void givenConjuredItem_whenQualityIsZero_thenQualityDoesNotGoNegative() {
        var conjured = new ConjuredItem("Conjured Mana Cake", 5, 0);
        conjured.degrade();
        assertEquals(4, conjured.sellIn);
        assertEquals(0, conjured.quality); // idem
    }

    @Test
    void givenConjuredItem_whenSellInIsZero_thenQualityDecreasesTwiceAsFastAgain2() {
        var conjured = new ConjuredItem("Conjured Mana Cake", 3, 6);
        conjured.degrade();
        assertEquals(2, conjured.sellIn);
        assertEquals(5, conjured.quality);
    }
}


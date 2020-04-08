package com.gildedrose;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class GildedRoseTest {

  private GildedRose gildedRose;

  @BeforeEach
  void setup() {
    gildedRose = new GildedRose(new Item[0]);
  }

  @Test
  void qualityDegradesWhenDateHasPassed() {
    Item item = new ItemBuilder().name("standard item").sellIn(2).quality(30).build();

    gildedRose.updateQuality(item);
    assertEquals(1, item.sellIn);
    assertEquals(29, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(0, item.sellIn);
    assertEquals(28, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-1, item.sellIn);
    assertEquals(26, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-2, item.sellIn);
    assertEquals(24, item.quality);
  }

  @Test
  void conjuredItemsDegradesTwiceAsFast() {
    Item item = ItemFixture.conjured().sellIn(2).quality(30).build();

    gildedRose.updateQuality(item);
    assertEquals(1, item.sellIn);
    assertEquals(28, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(0, item.sellIn);
    assertEquals(26, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-1, item.sellIn);
    assertEquals(24, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-2, item.sellIn);
    assertEquals(22, item.quality);
  }

  @Test
  void qualityNeverNegative() {
    Item item = ItemFixture.standard().sellIn(1).quality(1).build();

    gildedRose.updateQuality(item);
    assertEquals(0, item.sellIn);
    assertEquals(0, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-1, item.sellIn);
    assertEquals(0, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(-2, item.sellIn);
    assertEquals(0, item.quality);
  }

  @Test
  void agedBrieIncreasesQuality() {
    Item item = ItemFixture.agedBrie().sellIn(5).quality(10).build();

    gildedRose.updateQuality(item);
    assertEquals(4, item.sellIn);
    assertEquals(11, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(3, item.sellIn);
    assertEquals(12, item.quality);
  }

  @Test
  void qualityNeverBiggerThan50() {
    Item item = ItemFixture.agedBrie().sellIn(5).quality(49).build();
    gildedRose.updateQuality(item);
    assertEquals(50, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(50, item.quality);
  }

  @Test
  void qualityNeverBiggerThan50AtConstruction() {
    assertThrows(
        IllegalArgumentException.class, () -> ItemFixture.agedBrie().sellIn(5).quality(51).build());
  }

  @Test
  void qualityNeverBiggerThan50AtConstructionUnlessSulfuras() {
    ItemFixture.sulfuras().sellIn(5).build();
  }

  @Test
  void sulfurasIsNotSellable() {
    Item item = ItemFixture.sulfuras().sellIn(5).build();
    gildedRose.updateQuality(item);
    assertEquals(80, item.quality);
    assertEquals(5, item.sellIn);
  }

  @Test
  void backStagePasses() {
    Item item = ItemFixture.backstagePass().sellIn(12).quality(20).build();

    gildedRose.updateQuality(item);
    assertEquals(21, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(22, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(24, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(26, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(28, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(30, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(32, item.quality);

    item.quality = 20;
    gildedRose.updateQuality(item);
    assertEquals(23, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(26, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(29, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(32, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(35, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(0, item.quality);

    gildedRose.updateQuality(item);
    assertEquals(0, item.quality);
  }
}

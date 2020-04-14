package com.gildedrose;

public class ItemFixture {
  private static final String DEXTERITY_VEST = "+5 Dexterity Vest";
  private static final String AGED_BRIE = "Aged Brie";
  private static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
  private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
  private static final String CONJURED = "Conjured Mana Cake";

  static ItemBuilder standard() {
    return new ItemBuilder().name(DEXTERITY_VEST);
  }

  static ItemBuilder agedBrie() {
    return new ItemBuilder().name(AGED_BRIE);
  }

  static ItemBuilder backstagePass() {
    return new ItemBuilder().name(BACKSTAGE_PASS);
  }

  static ItemBuilder sulfuras() {
    return new ItemBuilder()
        .name(SULFURAS)
        .quality(80);
  }

  static ItemBuilder conjured() {
    return new ItemBuilder()
        .name(CONJURED);
  }
}

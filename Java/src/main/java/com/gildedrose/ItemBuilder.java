package com.gildedrose;

class ItemBuilder {

  private String name;
  private Category category;
  private int sellIn;
  private int quality;

  ItemBuilder() {}

  ItemBuilder name(String name) {
    this.name = name;
    this.category = Category.fromName(name);
    return this;
  }

  ItemBuilder sellIn(int sellIn) {
    this.sellIn = sellIn;
    return this;
  }

  ItemBuilder quality(int quality) {
    this.quality = quality;
    return this;
  }

  Item build() {
    if (name == null) {
      throw new IllegalArgumentException("name cannot be null");
    }
    if (Category.SULFURAS != category && quality > 50) {
      throw new IllegalArgumentException("quality cannot be higher than 50");
    } else if (Category.SULFURAS == category && quality != 80) {
      throw new IllegalArgumentException("Sulfuras quality must always be 80");
    }
    if (quality < 0) {
      throw new IllegalArgumentException("quality can not be negative");
    }
    return new Item(name, sellIn, quality);
  }
}

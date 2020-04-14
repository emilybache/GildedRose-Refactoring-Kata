package com.gildedrose;

import java.util.Arrays;

public enum Category {
  AGED_BRIE("^Aged Brie$"),
  BACKSTAGE_PASS("^Backstage passes to a TAFKAL80ETC concert$"),
  SULFURAS("^Sulfuras, Hand of Ragnaros$"),
  CONJURED("^Conjured Mana Cake$"),
  OTHER;

  private final String nameRegex;

  Category() {
    this(null);
  }

  Category(String nameRegex) {
    this.nameRegex = nameRegex;
  }

  private boolean matches(String actualName) {
    return nameRegex != null && actualName.matches(nameRegex);
  }

  static Category fromName(String name) {
    return Arrays.stream(Category.values()).filter(e -> e.matches(name)).findFirst().orElse(OTHER);
  }
}

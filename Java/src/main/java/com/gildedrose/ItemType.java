package com.gildedrose;

import java.util.Arrays;

public enum ItemType {
  AGED_BRIE("Aged Brie"),
  SULFURAS("Sulfuras, Hand of Ragnaros"),
  BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
  GENERIC("Generic item");

  private final String displayName;

  ItemType(String displayName) {
    this.displayName = displayName;
  }

  public static ItemType forDisplayName(String displayName) {
    return Arrays
      .stream(ItemType.values())
      .filter(itemType -> itemType.getDisplayName().equals(displayName)).findFirst().orElse(GENERIC);
  }

  public String getDisplayName() {
    return displayName;
  }
}

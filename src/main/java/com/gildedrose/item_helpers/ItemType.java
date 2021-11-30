package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;

public interface ItemType {

  void updateQuality();

  void validateQuality();

  String getName();

  String QUALITY_ERROR_MESSAGE = "Quality cannot be negative! Current value: ";
  String OUT_OF_BOUND_QUALITY_MESSAGE = "Quality cannot be above 50! Current value: ";
  String NOT_LEGENDARY_ITEM_ERROR_MESSAGE = "Item is legendary, quality must be always 80! Current value: ";

  static boolean qualityIsNegative(Item item) {
    return item.quality < 0;
  }

  static boolean qualityIsAbove50(Item item) {
    return item.quality > 50;
  }
}

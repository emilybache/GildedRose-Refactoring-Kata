package com.gildedrose.item_helpers;

import com.gildedrose.items.AgedBrieItem;
import com.gildedrose.items.BackstagePassItem;
import com.gildedrose.items.ConjuredItem;
import com.gildedrose.items.LegendaryItem;
import com.gildedrose.items.NormalItem;
import com.gildedrose.main.Item;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ItemFactory {

  private ItemFactory() {
  }

  public static ItemType getItemType(Item item) {
    QualityValidator.validateQuality(item);
    ItemType itemType = getItems(item).get(item.name);
    if (itemType == null) {
      itemType = new NormalItem(item);
    }
    return itemType;
  }

  private static Map<String, ItemType> getItems(Item item) {
    return Stream.of(new NormalItem(item), new AgedBrieItem(item), new LegendaryItem(item),
            new BackstagePassItem(item), new ConjuredItem(item))
        .collect(Collectors.toMap(ItemType::getName, itemType -> itemType));
  }

}

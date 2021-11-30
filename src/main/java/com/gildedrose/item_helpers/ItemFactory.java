package com.gildedrose.item_helpers;

import com.gildedrose.items.AgedBrieItem;
import com.gildedrose.items.BackstagePassItem;
import com.gildedrose.items.ConjuredItem;
import com.gildedrose.items.LegendaryItem;
import com.gildedrose.items.NormalItem;
import com.gildedrose.main.Item;

import java.util.Map;

import static java.util.stream.Collectors.toMap;
import static java.util.stream.Stream.of;

public class ItemFactory {

  private ItemFactory() {
  }

  public static ItemType getItemType(Item item) {
    ItemType itemType = getItems(item).get(item.name);
    if (itemType == null) {
      itemType = new NormalItem(item);
    }
    itemType.validateQuality();
    return itemType;
  }

  private static Map<String, ItemType> getItems(Item item) {
    return of(new NormalItem(item), new AgedBrieItem(item), new LegendaryItem(item),
        new BackstagePassItem(item), new ConjuredItem(item))
        .collect(toMap(ItemType::getName, itemType -> itemType));
  }

}

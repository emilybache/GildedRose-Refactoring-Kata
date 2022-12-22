package com.gildedrose;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

class GildedRose {
  private static final Map<ItemType, Class<? extends ItemHandler>> ITEM_HANDLER_MAP = Map.of(
    ItemType.AGED_BRIE,
    AgedBrieItemHandler.class,
    ItemType.SULFURAS,
    SulfurasItemHandler.class,
    ItemType.BACKSTAGE_PASSES,
    BackstagePassesItemHandler.class,
    ItemType.CONJURED,
    ConjuredItemHandler.class,
    ItemType.GENERIC,
    GenericItemHandler.class
  );
  // make package-private
  public final List<Item> items;

  public GildedRose(Item[] items) {
    this.items = Arrays.asList(items);
  }

  // todo: refactor this to use Guice
  private static void handleDay(Item item) {
    try {
      ItemHandler itemHandler = ITEM_HANDLER_MAP
        .get(ItemType.forDisplayName(item.name))
        .getDeclaredConstructor()
        .newInstance();
      itemHandler.handleDay(item);
    } catch (
      InstantiationException
      | IllegalAccessException
      | InvocationTargetException
      | NoSuchMethodException e
    ) {
      throw new RuntimeException(e);
    }
  }

  public void updateQuality() {
    items.forEach(GildedRose::handleDay);
  }
}

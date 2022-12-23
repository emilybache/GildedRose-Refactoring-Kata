package com.gildedrose;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;

public class ItemHandlerFactory {
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

  // todo: use guice
  public static ItemHandler getItemHandler(Item item) {
    try {
      return ITEM_HANDLER_MAP
        .get(ItemType.forDisplayName(item.name))
        .getDeclaredConstructor()
        .newInstance();
    } catch (
      InstantiationException
      | IllegalAccessException
      | InvocationTargetException
      | NoSuchMethodException e
    ) {
      throw new RuntimeException(e);
    }
  }
}

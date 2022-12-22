package com.gildedrose;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

class GildedRose {
  public static final String AGED_BRIE = "Aged Brie";
  public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
  public static final String BACKSTAGE_PASSES =
    "Backstage passes to a TAFKAL80ETC concert";
  private static Map<String, Class<? extends ItemHandler>> itemHandlerMap = Map.of(
    AGED_BRIE,
    AgedBrieItemHandler.class,
    SULFURAS,
    SulfurasItemHandler.class,
    BACKSTAGE_PASSES,
    BackstagePassesItemHandler.class
  );
  // make package-private
  public final List<Item> items;

  public GildedRose(Item[] items) {
    this.items = Arrays.asList(items);
  }

  // todo: refactor this to use Guice
  private static void handleDay(Item item) {
    // todo: refactor to use enum
    try {
      ItemHandler itemHandler = itemHandlerMap
        .getOrDefault(item.name, GenericItemHandler.class)
        .getDeclaredConstructor()
        .newInstance();
      itemHandler.handleDay(item);
    } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
      throw new RuntimeException(e);
    }
  }

  public void updateQuality() {
    items.forEach(GildedRose::handleDay);
  }
}

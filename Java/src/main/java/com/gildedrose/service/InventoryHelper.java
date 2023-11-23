package com.gildedrose.service;

import com.gildedrose.Inventory;
import com.gildedrose.Item;

import static com.gildedrose.Inventory.*;
import static java.util.stream.Stream.of;

public class InventoryHelper {
    private static final int MAX_QUALITY = 50;

    public static Inventory[] getInventoriesWithInvertedQualityDecrease() {
        return of(Inventory.values())
            .filter(Inventory::isQualityDecreaseInverted)
            .distinct()
            .toArray(Inventory[]::new);
    }

    public static void decreaseQualityAboveZeroItemsOtherThan(Item item, Inventory... inventories) {
        if (!includesItems(item, inventories)) {
            decreaseQualityAboveZero(item);
        } else if (itemNotLegendary(item)) item.quality = 0;
    }

    public static boolean includesItems(Item item, Inventory... inventories) {
        return of(inventories).anyMatch(inventory -> item.name.equals(inventory.getName()));
    }

    public static boolean itemNotLegendary(Item item) {
        return !item.name.equals(LEGENDARY.getName());
    }

    public static boolean itemAgedBrie(Item item) {
        return item.name.equals(AGED_BRIE.getName());
    }

    public static int getQualityDecreaseAmount(Item item) {
        return of(Inventory.values())
            .filter(inventory -> inventory.getName().equals(item.name))
            .findFirst()
            .map(Inventory::getQualityDecrease)
            .orElse(1);
    }

    public static void decreaseQualityAboveZero(Item item) {
        item.quality = item.quality > 0 ? item.quality - 1 : 0;
    }

    public static void increaseQualityBelowMaximum(Item item) {
        if (item.quality < MAX_QUALITY) {
            item.quality++;
        }
    }

    public static void increaseBackstagePass(Item item) {
        if (item.name.equals(BACKSTAGE_PASS.getName()) && item.sellIn < 10) {
            increaseQualityBelowMaximum(item);

            if (item.sellIn < 5) {
                increaseQualityBelowMaximum(item);
            }
        }
    }
}

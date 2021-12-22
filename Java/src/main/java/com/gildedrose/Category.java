package com.gildedrose;

import java.util.function.ToIntFunction;

public class Category {

    private final String name;
    private ToIntFunction<Item> updateQualityFunction;

    public Category(String name) {
        this.name = name;
    }

    public Category withQualityUpdater(ToIntFunction<Item> updateQualityFunction) {
        this.updateQualityFunction = updateQualityFunction;
        return this;
    }

    public boolean isItemMyCategory(String itemName) {
        return itemName != null && itemName.toLowerCase().startsWith(name.toLowerCase());
    }

    public void updateItem(Item item) {
        item.sellIn--;
        item.quality = Math.max(0, Math.min(50, updateQualityFunction.applyAsInt(item)));
    }
}

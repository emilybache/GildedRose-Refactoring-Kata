package com.gildedrose;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class QualityUpdater {

    // The 'categories' form the actual quality update specifications:
    private final Category[] categories = new Category[] {
        // "Aged Brie" actually increases in Quality the older it gets:
        new Category("Aged Brie")
            .withQualityUpdater(item -> item.quality + 1),

        // "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches.
        //	Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
        //	Quality drops to 0 after the concert:
        new Category("Backstage passes")
            .withQualityUpdater(item -> {
            if (item.sellIn < 0) {
                return 0;
            }
            if (item.sellIn < 5) {
                return item.quality + 3;
            }
            if (item.sellIn < 10) {
                return item.quality + 2;
            }
            return item.quality + 1;
        }),

        // "Sulfuras", being a legendary item, never has to be sold or decreases in Quality:
        new LegendaryCategory("Sulfuras"),

        // "Conjured" items degrade in Quality twice as fast as normal items:
        new Category("Conjured")
            .withQualityUpdater(new StandardQualityDegrader(2))
    };

    // Maps an Item name to a specific Category,
    // only to improve `findCategory()` in terms of speed:
    private final Map<String, Category> categoryByItemName = new HashMap<>();

    public void updateQuality(Item[] items) {
        Arrays.stream(items)
            .forEach(item -> categoryByItemName.computeIfAbsent(item.name, this::findCategory).updateItem(item));
    }

    private Category findCategory(String itemName) {
        return Arrays.stream(categories)
            .filter(category -> category.isItemMyCategory(itemName))
            .findFirst()
            .orElseGet(DefaultCategory::new);
    }
}

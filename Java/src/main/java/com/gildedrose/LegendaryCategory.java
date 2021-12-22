package com.gildedrose;

public class LegendaryCategory extends Category {

    public LegendaryCategory(String name) {
        super(name);
    }

    @Override
    public void updateItem(Item item) {
        // Legendary Items never alter
    }
}

package com.gildedrose;

public class StandardItemUpdateStrategy implements ItemUpdateStrategy {
    @Override
    public void update(Item item) {
        item.sellIn -= 1;
        if (item.quality > 0) {
            item.quality -= 1;
        }
        if (item.sellIn < 0 && item.quality > 0) {
            item.quality -= 1;
        }
    }
}

package com.gildedrose;

public class updateConjured extends ItemWrapper {
    public updateConjured(Item item) {
        super(item);
    }

    @Override
    public void update() {
        if (item.quality > 1) {
            item.quality = item.quality - 2;
        }
        item.sellIn = item.sellIn - 1;
        if (item.sellIn < 0 && item.quality > 1) {
            item.quality = item.quality - 2;
        }
    }
}

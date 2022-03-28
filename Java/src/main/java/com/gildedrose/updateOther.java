package com.gildedrose;

public class updateOther extends ItemWrapper {
    public updateOther(Item item) {
        super(item);
    }

    @Override
    public void update() {
        if (item.quality > 0) {
            item.quality = item.quality - 1;
        }
        item.sellIn = item.sellIn - 1;
        if (item.sellIn < 0 && item.quality > 0) {
            item.quality = item.quality - 1;
        }
    }
}

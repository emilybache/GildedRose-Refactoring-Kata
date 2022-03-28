package com.gildedrose;

public class updateBrie extends ItemWrapper {
    public updateBrie(Item item) {
        super(item);

    }

    @Override
    public void update() {
        if (item.quality < 50) {
            item.quality = item.quality + 1;
        }
        item.sellIn = item.sellIn - 1;
        if (item.sellIn < 0 && item.quality < 50) {
            item.quality = item.quality + 1;
        }
    }
}


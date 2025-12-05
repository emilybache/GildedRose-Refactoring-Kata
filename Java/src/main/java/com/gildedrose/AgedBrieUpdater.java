package com.gildedrose;

public class AgedBrieUpdater extends BaseItemUpdater {

    @Override
    public void update(Item item) {
        decreaseSellIn(item);
        if (item.sellIn < 0) {
            increaseQuality(item , 2);
        }
        else
            increaseQuality(item);
    }
}

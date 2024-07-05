package com.gildedrose;

public class SulfurasItemUpdater extends ItemUpdater {

    @Override
    public void update(Item item) {
        // Don't change legendary item
    }

    @Override
    int getIncrementValue() {
        return 0;
    }
}

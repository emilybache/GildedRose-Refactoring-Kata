package com.gildedrose.behavior.sellin;

import com.gildedrose.Item;

public class DefaultSellInBehavior implements SellInBehavior {

    @Override
    public void processSellInUpdate(Item item) {
        decreaseSellIn(item);
    }

    private void decreaseSellIn(Item item) {
        item.sellIn = item.sellIn - 1;
    }
}

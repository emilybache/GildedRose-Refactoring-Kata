package com.gildedrose.behavior.sellin;

import com.gildedrose.Item;

public class DefaultSellInBehavior implements SellInBehavior {

    public static DefaultSellInBehavior newInstance() {
        return new DefaultSellInBehavior();
    }

    @Override
    public void processSellInUpdate(Item item) {
        decreaseSellIn(item);
    }

    private void decreaseSellIn(Item item) {
        item.sellIn = item.sellIn - 1;
    }
}

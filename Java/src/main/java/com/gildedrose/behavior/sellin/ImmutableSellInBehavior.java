package com.gildedrose.behavior.sellin;

import com.gildedrose.Item;

public class ImmutableSellInBehavior implements SellInBehavior {

    public static ImmutableSellInBehavior newInstance() {
        return new ImmutableSellInBehavior();
    }

    @Override
    public void processSellInUpdate(Item item) {

    }
}

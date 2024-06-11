package com.gildedrose.rule;

import com.gildedrose.Item;

public class NormalRule implements Rule {
    @Override
    public boolean match(Item item) {
        return Goods.valueOfLabel(item.name) == null;
    }

    @Override
    public void apply(Item item) {
        item.sellIn--;
        decreaseQuality(item);
        if (item.sellIn < 0) {
            decreaseQuality(item);
        }
    }
}

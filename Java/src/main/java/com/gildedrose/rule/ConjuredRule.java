package com.gildedrose.rule;

import com.gildedrose.Item;

import static com.gildedrose.rule.Goods.CONJURED;

public class ConjuredRule implements Rule {
    @Override
    public boolean match(Item item) {
        return CONJURED.equals(Goods.valueOfLabel(item.name));
    }

    @Override
    public void apply(Item item) {
        item.sellIn--;
        decreaseQuality(item);
        decreaseQuality(item);
        if (item.sellIn < 0) {
            decreaseQuality(item);
            decreaseQuality(item);
        }

    }
}

package com.gildedrose.rule;

import com.gildedrose.Item;

import static com.gildedrose.rule.Goods.SULFURAS;

public class SulfurasRule implements Rule {
    @Override
    public boolean match(Item item) {
        return SULFURAS.equals(Goods.valueOfLabel(item.name));
    }

    @Override
    public void apply(Item item) {
        item.quality = 80;
    }
}

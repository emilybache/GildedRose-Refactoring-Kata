package com.gildedrose.rule;

import com.gildedrose.Item;

import static com.gildedrose.rule.Goods.AGED_BRIE;

public class AgedBrieRule implements Rule {
    @Override
    public boolean match(Item item) {
        return AGED_BRIE.equals(Goods.valueOfLabel(item.name));
    }

    @Override
    public void apply(Item item) {
        increaseQuality(item);
        item.sellIn -= 1;
        //code below doesn't match with requirements
        if (item.sellIn < 0) {
            increaseQuality(item);
        }
    }
}

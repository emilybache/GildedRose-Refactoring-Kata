package com.gildedrose.rule;

import com.gildedrose.Item;

import static com.gildedrose.rule.Goods.BACKSTAGE;

public class BackstageRule implements Rule{
    @Override
    public boolean match(Item item) {
        return BACKSTAGE.equals(Goods.valueOfLabel(item.name));
    }

    @Override
    public void apply(Item item) {
        increaseQuality(item);
        if (item.sellIn < 11) {
            increaseQuality(item);
        }
        if (item.sellIn < 6) {
            increaseQuality(item);
        }
        item.sellIn -= 1;
        if (item.sellIn < 0) {
            item.quality = 0;
        }
    }
}

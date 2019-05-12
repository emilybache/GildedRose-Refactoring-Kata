package com.gildedrose.items;

import com.gildedrose.BaseItem;
import com.gildedrose.Item;
import com.gildedrose.ItemInterface;

/**
 * Class Back Stage item inherited form NormalItem
 *
 * Business rules are inherited from NormalItem with more conditions:
 *  Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less
 */
public class BackStageItem extends BaseItem implements ItemInterface {
    public BackStageItem(Item item) {
        this.item=item;
    }

    public void updateQuality() {
        increaseQualityBy(1);
        if (item.sellIn < 10) {
            increaseQualityBy(1);
        }
        if (item.sellIn < 5) {
            increaseQualityBy(1);
        }
        if (itemHasExpired()) {
            item.quality -= item.quality;
        }
    }

    public void updateNumberOfdayToSellRemaining() {
        item.sellIn -= 1;
    }
}

package com.gildedrose.behavior.item;

import com.gildedrose.Item;
import com.gildedrose.behavior.quality.QualityBehavior;
import com.gildedrose.behavior.sellin.SellInBehavior;

public class ItemBehaviorImpl implements ItemBehavior {

    private final QualityBehavior qualityBehavior;
    private final SellInBehavior sellInBehavior;

    private ItemBehaviorImpl(QualityBehavior qualityBehavior, SellInBehavior sellInBehavior) {
        this.qualityBehavior = qualityBehavior;
        this.sellInBehavior = sellInBehavior;
    }

    public static ItemBehaviorImpl of(QualityBehavior qualityBehavior, SellInBehavior sellInBehavior) {
        return new ItemBehaviorImpl(qualityBehavior, sellInBehavior);
    }

    @Override
    public void processUpdate(Item item) {
        qualityBehavior.processQualityUpdate(item);
        sellInBehavior.processSellInUpdate(item);
    }
}

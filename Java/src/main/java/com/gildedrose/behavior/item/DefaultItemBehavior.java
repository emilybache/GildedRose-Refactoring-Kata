package com.gildedrose.behavior.item;

import com.gildedrose.Item;
import com.gildedrose.behavior.quality.DefaultQualityBehavior;
import com.gildedrose.behavior.sellin.DefaultSellInBehavior;
import com.gildedrose.behavior.quality.QualityBehavior;
import com.gildedrose.behavior.sellin.SellInBehavior;

public class DefaultItemBehavior implements ItemBehavior {

    private final QualityBehavior qualityBehavior = new DefaultQualityBehavior();
    private final SellInBehavior sellInBehavior = new DefaultSellInBehavior();

    @Override
    public void processUpdate(Item item) {
        qualityBehavior.processQualityUpdate(item);
        sellInBehavior.processSellInUpdate(item);
    }
}

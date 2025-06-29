package com.gildedrose.application.conjured;

import com.gildedrose.core.rule.UpdateInventoryTemplateRule;
import com.gildedrose.domain.item.ItemAdapter;

public class ConjuredRule extends UpdateInventoryTemplateRule {

    @Override
    protected boolean canSubtractSellIn(ItemAdapter itemAdapter) {
        return true;
    }

    @Override
    protected int getQualityFactor(boolean isExpired, ItemAdapter itemAdapter) {
        return 2;
    }

    @Override
    protected boolean canIncreaseQuality(boolean isExpired, ItemAdapter itemAdapter) {
        return false;
    }

    @Override
    protected boolean canDecreaseQuality(boolean isExpired, ItemAdapter itemAdapter) {
        return true;
    }
}

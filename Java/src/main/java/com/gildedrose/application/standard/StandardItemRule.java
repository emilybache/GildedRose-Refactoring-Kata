package com.gildedrose.application.standard;

import com.gildedrose.core.rule.UpdateInventoryTemplateRule;
import com.gildedrose.domain.item.ItemAdapter;

public class StandardItemRule extends UpdateInventoryTemplateRule {

    @Override
    protected boolean canSubtractSellIn(final ItemAdapter itemAdapter) {
        return true;
    }

    @Override
    protected int getQualityFactor(final boolean isExpired, final ItemAdapter itemAdapter) {
        return isExpired ? 2 : 1;
    }

    @Override
    protected boolean canIncreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return false;
    }

    @Override
    protected boolean canDecreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return true;
    }
}

package com.gildedrose.application.sulfuras;

import com.gildedrose.core.rules.UpdateInventoryTemplateRule;
import com.gildedrose.domain.item.ItemAdapter;

public class SulfurasRule extends UpdateInventoryTemplateRule {

    @Override
    protected boolean canSubtractSellIn(final ItemAdapter itemAdapter) {
        return false;
    }

    @Override
    protected int getQualityFactor(final boolean isExpired, final ItemAdapter itemAdapter) {
        return 0;
    }

    @Override
    protected boolean canIncreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return false;
    }

    @Override
    protected boolean canDecreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return false;
    }
}

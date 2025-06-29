package com.gildedrose.application.backstagepasses;

import com.gildedrose.core.rules.UpdateInventoryTemplateRule;
import com.gildedrose.domain.item.ItemAdapter;

public class BackstagePassesRule extends UpdateInventoryTemplateRule {

    final int SELL_IN_DAY10 = 10;
    final int SELL_IN_DAY5 = 5;

    @Override
    protected boolean canSubtractSellIn(final ItemAdapter itemAdapter) {
        return true;
    }

    @Override
    protected int getQualityFactor(final boolean isExpired, final ItemAdapter itemAdapter) {
        return isExpired ? itemAdapter.getItem().quality : checkSellInRange(itemAdapter);
    }

    @Override
    protected boolean canIncreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return !isExpired;
    }

    @Override
    protected boolean canDecreaseQuality(boolean isExpired, final ItemAdapter itemAdapter) {
        return isExpired;
    }

    private int checkSellInRange(final ItemAdapter itemAdapter) {
        int qualityFactor = 1;
        if (itemAdapter.getItem().sellIn <= SELL_IN_DAY5) {
            qualityFactor = 3;
        } else if (itemAdapter.getItem().sellIn <= SELL_IN_DAY10) {
            qualityFactor = 2;
        }
        return qualityFactor;
    }
}

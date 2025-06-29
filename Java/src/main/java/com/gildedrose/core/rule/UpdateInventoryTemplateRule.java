package com.gildedrose.core.rule;

import com.gildedrose.domain.item.ItemAdapter;

public abstract class UpdateInventoryTemplateRule {

    final int MINIMUM_QUALITY = 0;
    final int MAXIMUM_QUALITY = 50;
    final int SELL_IN_EXPIRED = 0;
    final int SELL_IN_UNIT = 1;

    public final void processItem(final ItemAdapter itemAdapter) {

        boolean isExpired = isExpired(itemAdapter);
        int qualityFactor = getQualityFactor(isExpired, itemAdapter);

        if (canIncreaseQuality(isExpired, itemAdapter)) {
            increaseQuality(itemAdapter, qualityFactor);
        }

        if (canSubtractSellIn(itemAdapter)) {
            subtractSellIn(itemAdapter);
        }

        if (canDecreaseQuality(isExpired, itemAdapter)) {
            decreaseQuality(itemAdapter, qualityFactor);
        }
    }

    protected abstract boolean canSubtractSellIn(final ItemAdapter itemAdapter);
    protected abstract int getQualityFactor(final boolean isExpired, final ItemAdapter itemAdapter);
    protected abstract boolean canIncreaseQuality(final boolean isExpired, final ItemAdapter itemAdapter);
    protected abstract boolean canDecreaseQuality(boolean isExpired, final ItemAdapter itemAdapter);

    private void subtractSellIn(final ItemAdapter itemAdapter) {
        itemAdapter.getItem().sellIn -= SELL_IN_UNIT;
    }

    private boolean isExpired(final ItemAdapter itemAdapter) {
        return itemAdapter.getItem().sellIn <= SELL_IN_EXPIRED;
    }

    private void increaseQuality(final ItemAdapter itemAdapter, final int qualityFactor) {
        int increasedQuality = itemAdapter.getItem().quality + qualityFactor;
        itemAdapter.getItem().quality = Math.min(increasedQuality, MAXIMUM_QUALITY);
    }

    private void decreaseQuality(final ItemAdapter itemAdapter, final int qualityFactor) {
        int decreasedQuality = itemAdapter.getItem().quality - qualityFactor;
        itemAdapter.getItem().quality = Math.max(decreasedQuality, MINIMUM_QUALITY);
    }
}

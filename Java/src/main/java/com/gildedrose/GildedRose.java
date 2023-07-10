package com.gildedrose;

import java.util.Arrays;
import java.util.List;
import java.util.Collections;

import com.gildedrose.item.quality.QualityUpdater;
import com.gildedrose.item.quality.QualityUpdaterMapper;

class GildedRose {
    private final List<Item> items;
    private final QualityUpdaterMapper qualityUpdaterMapper = new QualityUpdaterMapper();

    public GildedRose(Item[] items) {
        this.items = items != null ? Arrays.asList(items) : Collections.EMPTY_LIST;
    }

    public void updateQuality() {
        for (final Item item : items) {
            if (item != null) {
                final QualityUpdater updater = qualityUpdaterMapper.getQualityUpdater(item.name);
                updater.updateQuality(item);
            }
        }
    }
}

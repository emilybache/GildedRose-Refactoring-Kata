package com.gildedrose.service;

import com.gildedrose.Item;
import lombok.NoArgsConstructor;

import java.util.List;

import static com.gildedrose.enums.BorderQualities.MAX_QUALITY;
import static com.gildedrose.enums.BorderQualities.MIN_QUALITY;

@NoArgsConstructor
public class ConjuredQualityUpdater implements QualityUpdater {
    @Override
    public void updateQuality(List<Item> items) {
        for (Item item : items) {
            item.setSellIn(item.getSellIn() - 1);
            if (item.getQuality() < MAX_QUALITY.getQuality() &&
                item.getQuality() > MIN_QUALITY.getQuality() &&
                item.getQuality() - 2 >= MIN_QUALITY.getQuality()) {
                item.setQuality(item.getQuality() - 2);
            }
        }
    }
}

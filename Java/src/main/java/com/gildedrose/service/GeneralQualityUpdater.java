package com.gildedrose.service;

import com.gildedrose.Item;
import lombok.NoArgsConstructor;

import java.util.List;

import static com.gildedrose.enums.BorderDays.DATE_HAS_PASSED;
import static com.gildedrose.enums.BorderQualities.MAX_QUALITY;
import static com.gildedrose.enums.BorderQualities.MIN_QUALITY;

@NoArgsConstructor
public class GeneralQualityUpdater implements QualityUpdater {

    @Override
    public void updateQuality(List<Item> items) {
        for (Item item : items) {
            item.setSellIn(item.getSellIn() - 1);
            // at the end of each day decrease quantity and sellIn (quality > 0 and quantity <= 50 always)
            if (item.getSellIn() < DATE_HAS_PASSED.getDays() &&
                item.getQuality() - 2 >= MIN_QUALITY.getQuality()) {
                item.setQuality(item.getQuality() - 2);
            } else if (item.getQuality() < MAX_QUALITY.getQuality() && item.getQuality() > MIN_QUALITY.getQuality()) {
                item.setQuality(item.getQuality() - 1);
            }
        }
    }
}

package com.gildedrose.service;

import com.gildedrose.Item;
import lombok.NoArgsConstructor;

import java.util.List;

import static com.gildedrose.enums.BorderQualities.MAX_QUALITY;
import static com.gildedrose.enums.BorderQualities.MIN_QUALITY;

@NoArgsConstructor
public class AgedBrieQualityUpdater implements QualityUpdater {

    @Override
    public void updateQuality(List<Item> items) {
        for (Item item : items) {
            item.setSellIn(item.getSellIn() - 1);
            // quantity actually always increases as the item becomes older
            if (item.getQuality() < MAX_QUALITY.getQuality() && item.getQuality() >= MIN_QUALITY.getQuality()) {
                // Once the sell by date has passed, quality degrades twice as fast - should this have to happen, because it is logical contradiction ???
                /*if (item.getSellIn() < DATE_HAS_PASSED.getDays()) {
                    item.setQuality(item.getQuality() / 2);
                } else {*/
                if (item.getSellIn() < 0) {
                    item.setQuality(item.getQuality() + 2);
                } else {
                    item.setQuality(item.getQuality() + 1);
                }
            }
        }
    }
}
